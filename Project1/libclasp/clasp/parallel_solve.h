// 
// Copyright (c) 2010-2012, Benjamin Kaufmann
// 
// This file is part of Clasp. See http://www.cs.uni-potsdam.de/clasp/ 
// 
// Clasp is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// Clasp is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with Clasp; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//
#ifndef CLASP_PARALLEL_SOLVE_H_INCLUDED
#define CLASP_PARALLEL_SOLVE_H_INCLUDED

#ifdef _MSC_VER
#pragma once
#endif

//#if WITH_THREADS

#include "solve_algorithms.h"
#include "constraint.h"
#include "shared_context.h"
#include "util/thread.h"
#include "util/multi_queue.h"
#include "solver_types.h"

/*!
 * \file
 * Defines classes controlling multi-threaded parallel solving.
 *
 */
namespace Clasp {
	namespace mt {

		class ParallelHandler;
		class ParallelSolve;

#define CLASP_PARALLEL_SOLVE_OPTIONS(OPTION, SELF) \
	CLASP_BASIC_SOLVE_OPTIONS(OPTION, SELF) \
	OPTION("parallel-mode,t"   , parallel_mode, _0, "Run parallel search with given number of threads\n" \
		"      %A: <n {1..64}>[,<mode {compete|split}>]\n"                                                 \
		"        <n>   : Number of threads to use in search\n"                                             \
		"        <mode>: Run competition or splitting based search [compete]\n", {\
		PAIR(uint32, const char*) arg(1,"compete");\
		return store(value, arg) && X_SET_LEQ(SELF.algorithm.threads, arg.first, 64) && X_STORE_ENUM(SELF.algorithm.mode, arg.second, _2("compete", SolveOptions::Algorithm::mode_compete), _2("split", SolveOptions::Algorithm::mode_split));})\
	OPTION("global-restarts,@1", global_restarts, X(implicit("5")->arg("<X>")), "Configure global restart policy\n" \
		"      %A: <n>[,<sched>] / Implicit: %I\n"                         \
		"        <n> : Maximal number of global restarts (0=disable)\n"    \
		"     <sched>: Restart schedule [x,100,1.5] (<type {F|L|x|+}>)\n",{\
		PAIR(uint32, Clasp::ScheduleStrategy) arg;\
		return store(value, arg) && (SELF.restarts.sched=arg.second).type != ScheduleStrategy::user_schedule && X_SET(SELF.restarts.maxR, arg.first);})\
	OPTION("distribute!,@1"    , distribute, X(defaultsTo("conflict,4")), "Configure nogood distribution [%D]\n" \
		"      %A: <type>[,<lbd {0..127}>][,<size>]\n"                     \
		"        <type> : Distribute {all|short|conflict|loop} nogoods\n"  \
		"        <lbd>  : Distribute only if LBD  <= <lbd>  [4]\n"         \
		"        <size> : Distribute only if size <= <size> [-1]", {\
		PAIR(std::string, UPair) arg("", UPair(4, UINT32_MAX));\
		return (isDisabled(value) && (SELF.distribute=Distributor::Policy(0,0,0), true)) || (store(value, arg)\
			&& X_SET(SELF.distribute.lbd, arg.second.first) && X_SET_OR_FILL(SELF.distribute.size, arg.second.second) && X_STORE_ENUM_U(SELF.distribute.types, arg.first.c_str(),\
			_2("all", Distributor::Policy::all), _2("short", Distributor::Policy::implicit), _2("conflict", Distributor::Policy::conflict), _2("loop" , Distributor::Policy::loop)));})\
	OPTION("integrate,@1"      , integrate, X(defaultsTo("gp")->state(Value::value_defaulted)), "Configure nogood integration [%D]\n" \
		"      %A: <pick>[,<n>][,<topo>]\n"                                           \
		"        <pick>: Add {all|unsat|gp(unsat wrt guiding path)|active} nogoods\n" \
		"        <n>   : Always keep at least last <n> integrated nogoods   [1024]\n" \
		"        <topo>: Accept nogoods from {all|ring|cube|cubex} peers    [all]\n",{\
		PAIR(std::string, PAIR(uint32, const char*)) arg("", std::make_pair(1024, "all"));\
		return store(value, arg) && X_STORE_ENUM_U(SELF.integrate.filter, arg.first.c_str(), _2("all", SolveOptions::Integration::filter_no), _2("gp", SolveOptions::Integration::filter_gp),\
		_2("unsat", SolveOptions::Integration::filter_sat), _2("active", SolveOptions::Integration::filter_heuristic)) && X_SET_OR_FILL(SELF.integrate.grace, arg.second.first)\
		&& X_STORE_ENUM_U(SELF.integrate.topo, arg.second.second, _2("all" , SolveOptions::Integration::topo_all) , _2("ring" , SolveOptions::Integration::topo_ring),\
		_2("cube", SolveOptions::Integration::topo_cube), _2("cubex", SolveOptions::Integration::topo_cubex));})

		struct ParallelSolveOptions : BasicSolveOptions {
			typedef Distributor::Policy Distribution;
			ParallelSolveOptions() {}
			struct Algorithm {
				enum SearchMode { mode_split = 0, mode_compete = 1 };
				Algorithm() : threads(1), mode(mode_compete) {}
				uint32     threads;
				SearchMode mode;
			};
			struct Integration { /**< Nogood integration options. */
				static const uint32 GRACE_MAX = (1u << 28) - 1;
				Integration() : grace(1024), filter(filter_gp), topo(topo_all) {}
				enum Filter { filter_no = 0, filter_gp = 1, filter_sat = 2, filter_heuristic = 3 };
				enum Topology { topo_all = 0, topo_ring = 1, topo_cube = 2, topo_cubex = 3 };
				uint32 grace : 28; /**< Lower bound on number of shared nogoods to keep. */
				uint32 filter : 2;  /**< Filter for integrating shared nogoods (one of Filter). */
				uint32 topo : 2;  /**< Integration topology */
			};
			struct GRestarts {   /**< Options for configuring global restarts. */
				GRestarts() :maxR(0) {}
				uint32           maxR;
				ScheduleStrategy sched;
			};
			Integration  integrate; /**< Nogood integration parameters.     */
			Distribution distribute;/**< Nogood distribution parameters.    */
			GRestarts    restarts;  /**< Global restart strategy.           */
			Algorithm    algorithm; /**< Parallel algorithm to use.         */
			//! Allocates a new solve object.
			SolveAlgorithm* createSolveObject() const;
			//! Returns the number of threads that can run concurrently on the current hardware.
			static uint32   recommendedSolvers() { return Clasp::thread::hardware_concurrency(); }
			//! Returns number of maximal number of supported threads.
			static uint32   supportedSolvers() { return 64; }
			uint32          numSolver()        const { return algorithm.threads; }
			bool            defaultPortfolio() const { return algorithm.mode == Algorithm::mode_compete; }
		};

		//! A parallel algorithm for multi-threaded solving with and without search-space splitting.
		/*!
		 * The class adapts clasp's basic solve algorithm
		 * to a parallel solve algorithm that solves
		 * a problem using a given number of threads.
		 * It supports guiding path based solving, portfolio based solving, as well
		 * as a combination of these two approaches.
		 */
		class ParallelSolve : public SolveAlgorithm {
		public:
			explicit ParallelSolve(Enumerator* e, const ParallelSolveOptions& opts);
			~ParallelSolve();
			// base interface
			virtual bool interrupted() const;
			virtual void resetSolve();
			virtual void enableInterrupts();
			// own interface
			//! Returns the number of active threads.
			uint32 numThreads()            const;
			bool   integrateUseHeuristic() const { return test_bit(intFlags_, 31); }
			uint32 integrateGrace()        const { return intGrace_; }
			uint32 integrateFlags()        const { return intFlags_; }
			uint64 hasErrors()             const;
			//! Requests a global restart.
			void   requestRestart();
			bool   handleMessages(Solver& s);
			bool   integrateModels(Solver& s, uint32& mCount);
			void   pushWork(LitVec& gp);
			bool   commitModel(Solver& s);
			enum GpType { gp_none = 0, gp_split = 1, gp_fixed = 2 };
		private:
			ParallelSolve(const ParallelSolve&);
			ParallelSolve& operator=(const ParallelSolve&);
			typedef SingleOwnerPtr<const LitVec> PathPtr;
			enum ErrorCode { error_none = 0, error_oom = 1, error_runtime = 2, error_other = 4 };
			enum { masterId = 0 };
			// -------------------------------------------------------------------------------------------
			// Thread setup 
			struct EntryPoint;
			void   destroyThread(uint32 id);
			void   allocThread(uint32 id, Solver& s, const SolveParams& p);
			void   joinThreads();
			// -------------------------------------------------------------------------------------------
			// Algorithm steps
			void   setIntegrate(uint32 grace, uint8 filter);
			void   setRestarts(uint32 maxR, const ScheduleStrategy& rs);
			bool   beginSolve(SharedContext& ctx);
			bool   doSolve(SharedContext& ctx, const LitVec& assume);
			bool   doInterrupt();
			void   solveParallel(uint32 id);
			void   initQueue();
			bool   requestWork(Solver& s, PathPtr& out);
			void   terminate(Solver& s, bool complete);
			bool   waitOnSync(Solver& s);
			void   exception(uint32 id, PathPtr& path, ErrorCode e, const char* what);
			void   reportProgress(const Event& ev) const;
			// -------------------------------------------------------------------------------------------
			typedef ParallelSolveOptions::Distribution Distribution;
			struct SharedData;
			// SHARED DATA
			SharedData*       shared_;       // Shared control data
			ParallelHandler** thread_;       // Thread-locl control data
			// READ ONLY
			Distribution      distribution_; // distribution options
			uint32            maxRestarts_;  // disable global restarts once reached 
			uint32            intGrace_ : 30;// grace period for clauses to integrate
			uint32            intTopo_ : 2;// integration topology
			uint32            intFlags_;     // bitset controlling clause integration
			GpType            initialGp_;
		};

		struct MessageEvent : SolveEvent<MessageEvent> {
			enum Action { sent, received, completed };
			MessageEvent(const Solver& s, const char* message, Action a, double t = 0.0)
				: SolveEvent<MessageEvent>(s, verbosity_high), msg(message), time(t) { op = (uint32)a; }
			const char* msg;    // name of message
			double      time;   // only for action completed
		};
		//! A per-solver (i.e. thread) class that implements message handling and knowledge integration.
		/*!
		 * The class adds itself as a post propagator to the given solver. During propagation
		 * it checks for new messages and lemmas to integrate.
		 */
		class ParallelHandler : public MessageHandler {
		public:
			typedef ParallelSolve::GpType GpType;

			//! Creates a new parallel handler to be used in the given solve group.
			/*!
			 * \param ctrl The object controlling the parallel solve operation.
			 * \param s    The solver that is to be controlled by this object.
			 * \param p    The solver-specific solve options.
			 */
			explicit ParallelHandler(ParallelSolve& ctrl, Solver& s, const SolveParams& p);
			~ParallelHandler();
			//! Attaches the object's solver to ctx and adds this object as a post propagator.
			bool attach(SharedContext& ctx);
			//! Removes this object from the list of post propagators of its solver and detaches the solver from ctx.
			void detach(SharedContext& ctx, bool fastExit);

			void setError(int e) { error_ = e; }
			int  error() const { return (int)error_; }
			void setWinner() { win_ = 1; }
			bool winner() const { return win_ != 0; }
			void setThread(Clasp::thread& x) { assert(!joinable()); x.swap(thread_); assert(joinable()); }

			//! True if *this has an associated thread of execution, false otherwise.
			bool joinable() const { return thread_.joinable(); }
			//! Waits for the thread of execution associated with *this to finish.
			int join() { if (joinable()) { thread_.join(); } return error(); }

			// overridden methods

			//! Integrates new information.
			bool propagateFixpoint(Solver& s, PostPropagator*);
			bool handleMessages() { return ctrl_->handleMessages(solver()); }
			void reset() { up_ = 1; }
			bool simplify(Solver& s, bool re);
			//! Checks whether new information has invalidated current model.
			bool isModel(Solver& s);

			// own interface

			// TODO: make functions virtual once necessary 

			//! Returns true if handler's guiding path is disjoint from all others.
			bool disjointPath() const { return gp_.type == ParallelSolve::gp_split; }
			//! Returns true if handler has a guiding path.
			bool hasPath()      const { return gp_.type != ParallelSolve::gp_none; }
			void setGpType(GpType t) { gp_.type = t; }

			//! Entry point for solving the given guiding path.
			/*!
			 * \param solve   The object used for solving.
			 * \param gp      The new guiding path to assume.
			 * \param type    The guiding path's type.
			 * \param restart Request restart after restart number of conflicts.
			 */
			ValueRep solveGP(BasicSolve& solve, const LitVec& gp, GpType type, uint64 restart);

			/*!
			 * \name Message handlers
			 * \note
			 *   Message handlers are intended as callbacks for ParallelSolve::handleMessages().
			 *   They shall not change the assignment of the solver object.
			 */
			 //@{

			 //! Algorithm is about to terminate.
			 /*!
			  * Removes this object from the solver's list of post propagators.
			  */
			void handleTerminateMessage();

			//! Request for split.
			/*!
			 * Splits off a new guiding path and adds it to the control object.
			 * \pre The guiding path of this object is "splittable"
			 */
			void handleSplitMessage();

			//! Request for (global) restart.
			/*!
			 * \return true if restart is valid, else false.
			 */
			bool handleRestartMessage();

			Solver&            solver() { return *solver_; }
			const SolveParams& params() const { return *params_; }
			//@}  
		private:
			void add(ClauseHead* h);
			void clearDB(Solver* s);
			bool integrate(Solver& s);
			typedef LitVec::size_type size_type;
			typedef PodVector<Constraint*>::type ClauseDB;
			typedef SharedLiterals**             RecBuffer;
			enum { RECEIVE_BUFFER_SIZE = 32 };
			ParallelSolve*     ctrl_;       // my message source
			Solver*            solver_;     // my solver
			const SolveParams* params_;     // my solving params
			Clasp::thread      thread_;     // active thread or empty for master
			RecBuffer          received_;   // received clauses not yet integrated
			ClauseDB           integrated_; // my integrated clauses
			size_type          recEnd_;     // where to put next received clause
			size_type          intEnd_;     // where to put next clause
			uint32             error_ : 30;   // error code or 0 if ok
			uint32             win_ : 1;   // 1 if thread was the first to terminate the search
			uint32             up_ : 1;   // 1 if next propagate should check for new lemmas/models
			uint32             act_ : 1;   // 1 if gp is active
			struct GP {
				LitVec      path;     // current guiding path
				uint64      restart;  // don't give up before restart number of conflicts
				size_type   pos;      // pos in trail
				uint32      impl;     // number of additional implied literals
				uint32      modCount; // integration counter for synchronizing models
				GpType      type;    // type of gp
				void reset(uint64 r = UINT64_MAX, GpType t = ParallelSolve::gp_none) {
					path.clear();
					restart = r;
					pos = 0;
					impl = 0;
					modCount = 0;
					type = t;
				}
			} gp_;
		};

		class GlobalQueue : public Distributor {
		public:
			explicit GlobalQueue(const Policy& p, uint32 maxShare, uint32 topo);
			~GlobalQueue();
			uint32  receive(const Solver& in, SharedLiterals** out, uint32 maxOut);
			void    publish(const Solver& source, SharedLiterals* n);
		private:
			void release();
			struct DistPair {
				DistPair(uint32 sId = UINT32_MAX, SharedLiterals* x = 0) : sender(sId), lits(x) {}
				uint32          sender;
				SharedLiterals* lits;
			};
			class Queue : public MultiQueue<DistPair> {
			public:
				typedef MultiQueue<DistPair> base_type;
				using base_type::publish;
				Queue(uint32 m) : base_type(m) {}
			};
			struct ThreadInfo {
				Queue::ThreadId id;
				uint64          peerMask;
				char pad[64 - sizeof(Queue::ThreadId)];
			};
			Queue::ThreadId& getThreadId(uint32 sId) const { return threadId_[sId].id; }
			uint64           getPeerMask(uint32 sId) const { return threadId_[sId].peerMask; }
			uint64           populatePeerMask(uint32 topo, uint32 maxT, uint32 id) const;
			uint64           populateFromCube(uint32 maxT, uint32 id, bool ext) const;
			Queue*           queue_;
			ThreadInfo*      threadId_;
		};

	}
}
//#endif

#endif
