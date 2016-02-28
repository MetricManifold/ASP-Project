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
#ifndef CLASP_SHARED_CONTEXT_H_INCLUDED
#define CLASP_SHARED_CONTEXT_H_INCLUDED
#ifdef _MSC_VER
#pragma warning (disable : 4200) // nonstandard extension used : zero-sized array
#pragma once
#endif

#include "literal.h"
#include "constraint.h"
#include "util/left_right_sequence.h"
#include "util/misc_types.h"
#include "util/atomic.h"
#include "solver_strategies.h"
/*!
 * \file
 * Contains some types shared between different solvers
 */
namespace Clasp {
	class Solver;
	class ClauseInfo;
	class Assignment;
	class SharedContext;
	class SharedLiterals;
	class SharedDependencyGraph;
	struct SolverStats;

	/*!
	 * \addtogroup solver
	 */
	 //@{
	 //! Base class for preprocessors working on clauses only.
	class SatPreprocessor {
	public:
		//! A clause class optimized for preprocessing.
		class Clause {
		public:
			static Clause*  newClause(const Literal* lits, uint32 size);
			static uint64   abstractLit(Literal p) { return uint64(1) << ((p.var() - 1) & 63); }
			uint32          size()                const { return size_; }
			const Literal&  operator[](uint32 x)  const { return lits_[x]; }
			bool            inQ()                 const { return inQ_ != 0; }
			uint64          abstraction()         const { return data_.abstr; }
			Clause*         next()                const { return data_.next; }
			bool            marked()              const { return marked_ != 0; }
			Literal&        operator[](uint32 x) { return lits_[x]; }
			void            setInQ(bool b) { inQ_ = (uint32)b; }
			void            setMarked(bool b) { marked_ = (uint32)b; }
			uint64&         abstraction() { return data_.abstr; }
			Clause*         linkRemoved(Clause* next) { data_.next = next; return this; }
			void            strengthen(Literal p);
			void            simplify(Solver& s);
			void            destroy();
		private:
			Clause(const Literal* lits, uint32 size);
			union {
				uint64  abstr;      // abstraction of literals
				Clause* next;       // next removed clause
			}       data_;
			uint32  size_ : 30; // size of the clause
			uint32  inQ_ : 1;  // in todo-queue?
			uint32  marked_ : 1;  // a marker flag
			Literal lits_[1];     // literals of the clause: [lits_[0], lits_[size_])
		};

		SatPreprocessor() : ctx_(0), elimTop_(0) {}
		virtual ~SatPreprocessor();

		//! Creates a clone of this preprocessor.
		/*!
		 * \note The function does not clone any clauses already added to *this.
		 */
		virtual SatPreprocessor* clone() = 0;

		uint32 numClauses() const { return (uint32)clauses_.size(); }
		//! Adds a clause to the preprocessor.
		/*!
		 * \pre clause is not a tautology (i.e. does not contain both l and ~l)
		 * \pre clause is a set (i.e. does not contain l more than once)
		 * \return true if clause was added. False if adding the clause makes the problem UNSAT
		 */
		bool addClause(const LitVec& cl) { return addClause(!cl.empty() ? &cl[0] : 0, cl.size()); }
		bool addClause(const Literal* clause, uint32 size);
		//! Runs the preprocessor on all clauses that were previously added.
		/*!
		 * \pre A ctx.startAddConstraint() was called and has variables for all added clauses.
		 */
		bool preprocess(SharedContext& ctx, SatPreParams& opts);
		bool preprocess(SharedContext& ctx);

		//! Force removal of state & clauses.
		void cleanUp(bool discardEliminated = false);

		//! Extends the model in m with values for any eliminated variables.
		void extendModel(ValueVec& m, LitVec& open);
		struct Stats {
			Stats() : clRemoved(0), clAdded(0), litsRemoved(0) {}
			uint32 clRemoved;
			uint32 clAdded;
			uint32 litsRemoved;
		} stats;
		typedef SatPreParams Options;
	protected:
		typedef PodVector<Clause*>::type  ClauseList;
		virtual bool  initPreprocess(SatPreParams& opts) = 0;
		virtual bool  doPreprocess() = 0;
		virtual void  doExtendModel(ValueVec& m, LitVec& open) = 0;
		virtual void  doCleanUp() = 0;
		Clause*       clause(uint32 clId) { return clauses_[clId]; }
		const Clause* clause(uint32 clId) const { return clauses_[clId]; }
		void          discardClauses(bool discardEliminated);
		void          setClause(uint32 clId, const LitVec& cl) {
			clauses_[clId] = Clause::newClause(&cl[0], (uint32)cl.size());
		}
		void          destroyClause(uint32 clId) {
			clauses_[clId]->destroy();
			clauses_[clId] = 0;
			++stats.clRemoved;
		}
		void          eliminateClause(uint32 id) {
			elimTop_ = clauses_[id]->linkRemoved(elimTop_);
			clauses_[id] = 0;
			++stats.clRemoved;
		}
		SharedContext*  ctx_;     // current context
		Clause*         elimTop_; // stack of blocked/eliminated clauses
	private:
		SatPreprocessor(const SatPreprocessor&);
		SatPreprocessor& operator=(const SatPreprocessor&);
		ClauseList      clauses_; // initial non-unit clauses
		LitVec          units_;   // initial unit clauses
	};
	//@}

	/**
	 * \defgroup shared classes to be shared between solvers
	 */
	 //@{

	 ///////////////////////////////////////////////////////////////////////////////
	 // Problem statistics
	 ///////////////////////////////////////////////////////////////////////////////
	 //! A struct for aggregating basic problem statistics.
	 /*!
	  * Maintained in SharedContext.
	  */
	struct ProblemStats {
		ProblemStats() { reset(); }
		uint32  vars;
		uint32  vars_eliminated;
		uint32  vars_frozen;
		uint32  constraints;
		uint32  constraints_binary;
		uint32  constraints_ternary;
		uint32  complexity;
		void    reset() { std::memset(this, 0, sizeof(*this)); }
		uint32  numConstraints() const { return constraints + constraints_binary + constraints_ternary; }
		void diff(const ProblemStats& o) {
			vars = std::max(vars, o.vars) - std::min(vars, o.vars);
			vars_eliminated = std::max(vars_eliminated, o.vars_eliminated) - std::min(vars_eliminated, o.vars_eliminated);
			vars_frozen = std::max(vars_frozen, o.vars_frozen) - std::min(vars_frozen, o.vars_frozen);
			constraints = std::max(constraints, o.constraints) - std::min(constraints, o.constraints);
			constraints_binary = std::max(constraints_binary, o.constraints_binary) - std::min(constraints_binary, o.constraints_binary);
			constraints_ternary = std::max(constraints_ternary, o.constraints_ternary) - std::min(constraints_ternary, o.constraints_ternary);
		}
		double operator[](const char* key) const;
		static const char* keys(const char* = 0);
	};

	//! Stores static information about a variable.
	struct VarInfo {
		enum FLAG {
			MARK_P = 0x1u, // mark for positive literal
			MARK_N = 0x2u, // mark for negative literal
			NANT = 0x4u, // var in NAnt(P)?
			PROJECT = 0x8u, // do we project on this var?
			BODY = 0x10u,// is this var representing a body?
			EQ = 0x20u,// is the var representing both a body and an atom?
			DISJ = 0x40u,// in non-hcf disjunction?
			FROZEN = 0x80u // is the variable frozen?
		};
		VarInfo() : rep(0) {}
		bool  has(FLAG f)  const { return (rep & flag(f)) != 0; }
		bool  has(uint32 f)const { return (rep & f) != 0; }
		void  set(FLAG f) { rep |= flag(f); }
		void  toggle(FLAG f) { rep ^= flag(f); }
		static uint8 flag(FLAG x) { return uint8(x); }
		uint8 rep;
	};

	//! A class for efficiently storing and propagating binary and ternary clauses.
	class ShortImplicationsGraph {
	public:
		ShortImplicationsGraph();
		~ShortImplicationsGraph();
		//! Makes room for nodes number of nodes.
		void resize(uint32 nodes);
		//! Mark the instance as shared/unshared.
		/*!
		 * A shared instance adds learnt binary/ternary clauses
		 * to specialized shared blocks of memory.
		 */
		void markShared(bool b) { shared_ = b; }
		//! Adds the binary constraint (p, q) to the implication graph.
		/*!
		 * \return true iff a new implication was added.
		 */
		bool addBinary(Literal p, Literal q, bool learnt);
		//! Adds the ternary constraint (p, q, r) to the implication graph.
		/*!
		 * \return true iff a new implication was added.
		 */
		bool addTernary(Literal p, Literal q, Literal r, bool learnt);

		//! Removes p and its implications.
		/*!
		 * \pre s.isTrue(p)
		 */
		void removeTrue(const Solver& s, Literal p);

		//! Propagates consequences of p following from binary and ternary clauses.
		/*!
		 * \pre s.isTrue(p)
		 */
		bool   propagate(Solver& s, Literal p) const;
		//! Propagates immediate consequences of p following from binary clauses only.
		bool   propagateBin(Assignment& out, Literal p, uint32 dl) const;
		//! Checks whether there is a reverse arc implying p and if so returns it in out.
		bool   reverseArc(const Solver& s, Literal p, uint32 maxLev, Antecedent& out) const;

		uint32 numBinary() const { return bin_[0]; }
		uint32 numTernary()const { return tern_[0]; }
		uint32 numLearnt() const { return bin_[1] + tern_[1]; }
		uint32 numEdges(Literal p) const;
		//! Applies op on all unary- and binary implications following from p.
		/*!
		 * OP must provide two functions:
		 *  - bool unary(Literal, Literal)
		 *  - bool binary(Literal, Literal, Literal)
		 * The first argument will be p, the second (resp. third) the unary
		 * (resp. binary) clause implied by p.
		 * \note For learnt imps, at least one literal has its watch-flag set.
		 */
		template <class OP>
		bool forEach(Literal p, const OP& op) const {
			const ImplicationList& x = graph_[p.index()];
			if (x.empty()) return true;
			ImplicationList::const_right_iterator rEnd = x.right_end(); // prefetch
			for (ImplicationList::const_left_iterator it = x.left_begin(), end = x.left_end(); it != end; ++it) {
				if (!op.unary(p, *it)) { return false; }
			}
			for (ImplicationList::const_right_iterator it = x.right_begin(); it != rEnd; ++it) {
				if (!op.binary(p, it->first, it->second)) { return false; }
			}
#if WITH_THREADS
			for (Block* b = (x).learnt; b; b = b->next) {
				p.watch(); bool r = true;
				for (Block::const_iterator imp = b->begin(), endOf = b->end(); imp != endOf; ) {
					if (!imp->watched()) { r = op.binary(p, imp[0], imp[1]); imp += 2; }
					else { r = op.unary(p, imp[0]);          imp += 1; }
					if (!r) { return false; }
				}
			}
#endif
			return true;
		}
	private:
		ShortImplicationsGraph(const ShortImplicationsGraph&);
		ShortImplicationsGraph& operator=(ShortImplicationsGraph&);
		struct Propagate;
		struct ReverseArc;
#if WITH_THREADS
		struct Block {
			typedef Clasp::atomic<uint32> atomic_size;
			typedef Clasp::atomic<Block*> atomic_ptr;
			typedef const Literal*      const_iterator;
			typedef       Literal*      iterator;
			enum { block_cap = (64 - (sizeof(atomic_size) + sizeof(atomic_ptr))) / sizeof(Literal) };
			explicit Block();
			const_iterator  begin() const { return data; }
			const_iterator  end()   const { return data + size(); }
			iterator        end() { return data + size(); }
			uint32          size()  const { return size_lock >> 1; }
			bool tryLock(uint32& lockedSize);
			void addUnlock(uint32 lockedSize, const Literal* x, uint32 xs);
			atomic_ptr  next;
			atomic_size size_lock;
			Literal     data[block_cap];
		};
		typedef Block::atomic_ptr SharedBlockPtr;
		typedef bk_lib::left_right_sequence<Literal, std::pair<Literal, Literal>, 64 - sizeof(SharedBlockPtr)> ImpListBase;
		struct ImplicationList : public ImpListBase {
			ImplicationList() : ImpListBase() { learnt = 0; }
			ImplicationList(const ImplicationList& other) : ImpListBase(other), learnt(other.learnt) {}
			~ImplicationList();
			bool hasLearnt(Literal q, Literal r = negLit(0)) const;
			void addLearnt(Literal q, Literal r = negLit(0));
			void simplifyLearnt(const Solver& s);
			bool empty() const { return ImpListBase::empty() && learnt == 0; }
			void move(ImplicationList& other);
			void clear(bool b);
			SharedBlockPtr learnt;
		};
#else
		typedef bk_lib::left_right_sequence<Literal, std::pair<Literal, Literal>, 64> ImplicationList;
#endif
		ImplicationList& getList(Literal p) { return graph_[p.index()]; }
		void remove_bin(ImplicationList& w, Literal p);
		void remove_tern(ImplicationList& w, Literal p);
		typedef PodVector<ImplicationList>::type ImpLists;
		ImpLists   graph_;     // one implication list for each literal
		uint32     bin_[2];    // number of binary constraints (0: problem / 1: learnt)
		uint32     tern_[2];   // number of ternary constraints(0: problem / 1: learnt)
		bool       shared_;
	};

	//! Base class for distributing learnt knowledge between solvers.
	class Distributor {
	public:
		struct Policy {
			enum Types {
				no = 0,
				conflict = Constraint_t::learnt_conflict,
				loop = Constraint_t::learnt_loop,
				all = conflict | loop,
				implicit = all + 1
			};
			Policy(uint32 a_sz = 0, uint32 a_lbd = 0, uint32 a_type = 0) : size(a_sz), lbd(a_lbd), types(a_type) {}
			uint32 size : 22; /*!< Allow distribution up to this size only. */
			uint32 lbd : 7; /*!< Allow distribution up to this lbd only.  */
			uint32 types : 3; /*!< Restrict distribution to these types.    */
		};
		static  uint64  mask(uint32 i) { return uint64(1) << i; }
		static  uint32  initSet(uint32 sz) { return (uint64(1) << sz) - 1; }
		static  bool    inSet(uint64 s, uint32 id) { return (s & mask(id)) != 0; }
		explicit Distributor(const Policy& p);
		virtual ~Distributor();
		bool            isCandidate(uint32 size, uint32 lbd, uint32 type) const {
			return size <= policy_.size && lbd <= policy_.lbd && ((type & policy_.types) != 0);
		}
		virtual void    publish(const Solver& source, SharedLiterals* lits) = 0;
		virtual uint32  receive(const Solver& in, SharedLiterals** out, uint32 maxOut) = 0;
	private:
		Distributor(const Distributor&);
		Distributor& operator=(const Distributor&);
		Policy policy_;
	};

	//! Aggregates information to be shared between solver objects.
	/*!
	 * Among other things, SharedContext objects store
	 * static information on variables, the (possibly unused)
	 * symbol table, as well as the binary and ternary
	 * implication graph of the input problem.
	 *
	 * Furthermore, a SharedContext object always stores a distinguished
	 * master solver that is used to store and simplify problem constraints.
	 * Additional solvers can be added via SharedContext::addSolver().
	 * Once initialization is completed, any additional solvers must be attached
	 * to this object by calling SharedContext::attach().
	 */
	class SharedContext {
	public:
		typedef SharedDependencyGraph SDG;
		typedef PodVector<Solver*>::type       SolverVec;
		typedef SingleOwnerPtr<SDG>            SccGraph;
		typedef Configuration*                 ConfigPtr;
		typedef std::auto_ptr<Distributor>     DistrPtr;
		typedef ProblemStats                   Stats;
		typedef LitVec::size_type              size_type;
		typedef ShortImplicationsGraph         BTIG;
		typedef EventHandler*                  LogPtr;
		typedef std::auto_ptr<SatPreprocessor> SatPrePtr;
		enum InitMode { init_share_symbols, init_copy_symbols };
		/*!
		 * \name Configuration
		 */
		 //@{
		 //! Creates a new object for sharing variables and the binary and ternary implication graph.
		explicit SharedContext();
		~SharedContext();
		//! Enables event reporting via the given event handler.
		void       eventHandler(EventHandler* r) { progress_ = r; }
		LogPtr     eventHandler() const { return progress_; }
		//! Sets how to handle physical sharing of constraints.
		void       setShareMode(ContextParams::ShareMode m);
		//! Sets whether the short implication graph should be used for storing short learnt constraints.
		void       setShortMode(ContextParams::ShortMode m);
		//! Resets this object to the state after default construction.
		void       reset();
		//! Sets maximal number of solvers sharing this object.
		void       concurrency(uint32 numSolver);
		//! Adds an additional solver to this object and returns it.
		Solver&    addSolver();
		bool       hasSolver(uint32 id) const { return id < solvers_.size(); }
		bool       seedSolvers()        const { return share_.seed != 0; }
		//! Configures the statistic object of attached solvers.
		/*!
		 * The level determines the amount of extra statistics.
		 * Currently two levels are supported:
		 *  - Level 1 enables ExtendedStats
		 *  - Level 2 enables ExtendedStats and JumpStats
		 * \see ExtendedStats
		 * \see JumpStats
	   */
		void       enableStats(uint32 level);
		void       accuStats();
		//! Sets the configuration for this object and its attached solvers.
		/*!
		 * \note If own is true, ownership of c is transferred.
		 */
		void       setConfiguration(Configuration* c, bool own);

		SatPrePtr  satPrepro;  /*!< Preprocessor for simplifying problem.                  */
		DistrPtr   distributor;/*!< Distributor object to use for distribution of learnt constraints. */
		SccGraph   sccGraph;   /*!< Program dependency graph - only used for ASP-problems. */
		//! Returns true unless the master has an unresolvable top-level conflict.
		bool       ok()                            const;
		//! Returns the master solver associated with this object.
		Solver*    master()                        const { return solver(0); }
		//! Returns the solver with the given id.
		Solver*    solver(uint32 id)               const { return solvers_[id]; }
		uint32     concurrency()                   const { return share_.count; }
		bool       frozen()                        const { return share_.frozen; }
		bool       isShared()                      const { return frozen() && concurrency() > 1; }
		bool       physicalShare(ConstraintType t) const { return (share_.shareM & (1 + (t != Constraint_t::static_constraint))) != 0; }
		bool       physicalShareProblem()          const { return (share_.shareM & ContextParams::share_problem) != 0; }
		bool       allowImplicit(ConstraintType t) const { return t != Constraint_t::static_constraint ? share_.shortM != ContextParams::short_explicit : !isShared(); }
		bool       preserveModels()                const { return static_cast<SatPreParams::Mode>(share_.satPreM) == SatPreParams::prepro_preserve_models; }
		bool       isExtended()                    const { return problem_.vars_frozen || symTab().type() == SymbolTable::map_indirect; }
		//! Returns the current configuration used in this object.
		/*!
		 * If no configuration was set, a default configuration is used.
		 * \see DefaultConfiguration
		 */
		ConfigPtr  configuration()                 const { return config_.get(); }
		//@}

		/*!
		 * \name Problem specification
		 * Functions for adding a problem to the master solver.
		 * Problem specification is a four-stage process:
		 * -# Add variables to the SharedContext object.
		 * -# Call startAddConstraints().
		 * -# Add problem constraints to the master solver.
		 * -# Call endInit() to finish the initialization process.
		 * .
		 * \note After endInit() was called, other solvers can be attached to this object.
		 * \note In incremental setting, the process must be repeated for each incremental step.
		 *
		 * \note Problem specification is *not* thread-safe, i.e. during initialization no other thread shall
		 * access the context.
		 *
		 * \note !frozen() is a precondition for all functions in this group!
		 */
		 //@{
		bool    unfreeze();
		//! Reserves space for at least varGuess variables.
		/*!
		 * \param varGuess Number of vars to reserve space for.
		 * \note If the number of variables is known upfront, passing the correct value
		 * for varGuess avoids repeated regrowing of the state data structures.
		 */
		void    reserveVars(uint32 varGuess);
		//! Clones vars and symbol table from other.
		void    cloneVars(const SharedContext& other, InitMode m = init_copy_symbols);
		//! Removes all vars [startVar, numVars()].
		void    popVars(uint32 startVar) { varInfo_.resize(std::max(Var(1), startVar)); problem_.vars = numVars(); }

		//! Adds a new variable of type t.
		/*!
		 * \param t  Type of the new variable (either Var_t::atom_var or Var_t::body_var).
		 * \param eq True if var represents both an atom and a body. In that case
		 *           t is the variable's primary type and determines the preferred literal.
		 * \return The index of the new variable.
		 * \note Problem variables are numbered from 1 onwards!
		 */
		Var     addVar(VarType t, bool eq = false);
		//! Request additional reason data slot for variable v.
		void    requestData(Var v);
		//! Freezes/defreezes a variable (a frozen var is exempt from SatELite preprocessing).
		void    setFrozen(Var v, bool b);
		//! Adds v to resp. removes v from the set of projection variables.
		void    setProject(Var v, bool b) { assert(validVar(v)); if (b != varInfo_[v].has(VarInfo::PROJECT)) varInfo_[v].toggle(VarInfo::PROJECT); }
		//! Marks/unmarks v as contained in a negative loop or head of a choice rule.
		void    setNant(Var v, bool b) { assert(validVar(v)); if (b != varInfo_[v].has(VarInfo::NANT))    varInfo_[v].toggle(VarInfo::NANT); }
		//! Mark/unmark v as contained in a non-hcf disjunction.
		void    setInDisj(Var v, bool b) { assert(validVar(v)); if (b != varInfo_[v].has(VarInfo::DISJ))    varInfo_[v].toggle(VarInfo::DISJ); }
		//! Eliminates the variable v.
		/*!
		 * \pre v must not occur in any constraint and frozen(v) == false and value(v) == value_free
		 */
		void    eliminate(Var v);
		void    setVarEq(Var v, bool b) { assert(validVar(v)); if (b != varInfo_[v].has(VarInfo::EQ))      varInfo_[v].toggle(VarInfo::EQ); }
		void    mark(Literal p) { assert(validVar(p.var())); varInfo_[p.var()].rep |= (VarInfo::MARK_P + p.sign()); }
		void    unmark(Var v) { assert(validVar(v)); varInfo_[v].rep &= ~(VarInfo::MARK_P | VarInfo::MARK_N); }
		void    setPreserveModels() { share_.satPreM = SatPreParams::prepro_preserve_models; }
		//! Requests a special tag literal for tagging conditional knowledge.
		/*!
		 * Once a tag literal p is set, learnt clauses containing ~p are
		 * tagged as "conditional". Conditional clauses can be removed from a solver
		 * by calling Solver::removeConditional(). Furthermore, calling
		 * Solver::strengthenConditional() removes ~p from conditional clauses and
		 * transforms them to unconditional knowledge.
		 *
		 * \note Typically, the tag literal is an initial assumption and hence true during
		 *       the whole search.
		 */
		void    requestTagLiteral();
		//! Requests a special literal for tagging enumeration/step-related knowledge.
		void    requestStepLiteral();

		//! Returns the number of problem variables.
		/*!
		 * \note The special sentinel-var 0 is not counted, i.e. numVars() returns
		 * the number of problem-variables.
		 * To iterate over all problem variables use a loop like:
		 * \code
		 * for (Var i = 1; i <= numVars(); ++i) {...}
		 * \endcode
		 */
		uint32  numVars()          const { return static_cast<uint32>(varInfo_.size() - 1); }
		//! Returns the number of eliminated vars.
		uint32  numEliminatedVars()const { return problem_.vars_eliminated; }

		//! Returns true if var represents a valid variable in this object.
		/*!
		 * \note The range of valid variables is [1..numVars()]. The variable 0
		 * is a special sentinel variable.
		 */
		bool    validVar(Var var)  const { return var <= numVars(); }
		//! Returns the type of variable v.
		/*!
		 * If v was added with parameter eq=true, the return value
		 * is Var_t::atom_body_var.
		 */
		VarType type(Var v)        const {
			assert(validVar(v));
			return varInfo_[v].has(VarInfo::EQ)
				? Var_t::atom_body_var
				: VarType(Var_t::atom_var + varInfo_[v].has(VarInfo::BODY));
		}
		//! Returns true if v is currently eliminated, i.e. no longer part of the problem.
		bool    eliminated(Var v)  const;
		//! Returns true if v is excluded from variable elimination.
		bool    frozen(Var v)      const { assert(validVar(v)); return varInfo_[v].has(VarInfo::FROZEN); }
		//! Returns true if v is a projection variable.
		bool    project(Var v)     const { assert(validVar(v)); return varInfo_[v].has(VarInfo::PROJECT); }
		//! Returns true if v is contained in a negative loop or head of a choice rule.
		bool    nant(Var v)        const { assert(validVar(v)); return varInfo_[v].has(VarInfo::NANT); }
		bool    inDisj(Var v)      const { assert(validVar(v)); return varInfo_[v].has(VarInfo::DISJ); }
		Literal tagLiteral()       const { return tag_; }
		Literal stepLiteral()      const { return step_; }
		bool    marked(Literal p)  const { assert(validVar(p.var())); return varInfo_[p.var()].has(VarInfo::MARK_P + p.sign()); }
		VarInfo info(Var v)        const { assert(validVar(v)); return varInfo_[v]; }
		//! Returns the preferred decision literal of variable v w.r.t its type.
		/*!
		 * \return
		 *  - posLit(v) if type(v) == body_var
		 *  - negLit(v) if type(v) == atom_var
		 * \note If type(v) is atom_body_var, the preferred literal is determined
		 *       by v's primary type, i.e. the one that was initially passed to addVar().
		 */
		Literal preferredLiteralByType(Var v) const {
			assert(validVar(v));
			return Literal(v, !varInfo_[v].has(VarInfo::BODY));
		}

		//! Prepares the master solver so that constraints can be added.
		/*!
		 * Must be called to publish previously added variables to master solver
		 * and before constraints over these variables can be added.
		 * \post !frozen()
		 * \return The master solver associated with this object.
		 */
		Solver& startAddConstraints(uint32 constraintGuess = 100);
		//! Same as master()->addUnary(p, Constraint_t::static_constraint)
		bool    addUnary(Literal p);
		//! Same as master()->add(c)
		void    add(Constraint* c);
		//! Same as shortImplications->addBinary(p, q, learnt).
		bool    addBinary(Literal p, Literal q, bool learnt = false) { return btig_.addBinary(p, q, learnt); }
		//! Same as shortImplications->addTernary(p, q, learnt).
		bool    addTernary(Literal p, Literal q, Literal r, bool learnt = false) { return btig_.addTernary(p, q, r, learnt); }

		//! Finishes initialization of the master solver.
		/*!
		 * The function must be called once before search is started. After endInit()
		 * was called, previously added solvers can be attached to the
		 * shared context and learnt constraints may be added to solver.
		 * \param attachAll If true, also calls attach() for all solvers that were added to this object.
		 * \return If the constraints are initially conflicting, false. Otherwise, true.
		 * \note
		 * The master solver can't recover from top-level conflicts, i.e. if endInit()
		 * returned false, the solver is in an unusable state.
		 * \post frozen()
		 */
		bool    endInit(bool attachAll = false);

		//! Attaches the solver with the given id to this object.
		/*!
		 * \note It is safe to attach multiple solvers concurrently
		 * but the master solver shall not change during the whole
		 * operation.
		 *
		 * \pre id < concurrency()
		 */
		bool    attach(uint32 id) { return attach(*solver(id)); }
		bool    attach(Solver& s);

		//! Detaches the solver with the given id from this object.
		/*!
		 * The function removes any tentative constraints from s.
		 * Shall be called once after search has stopped.
		 * \note The function is concurrency-safe w.r.t to different solver objects,
		 *       i.e. in a parallel search different solvers may call detach()
		 *       concurrently.
		 */
		void    detach(uint32 id, bool reset = false) { return detach(*solver(id), reset); }
		void    detach(Solver& s, bool reset = false);

		//! Returns the number of problem constraints.
		uint32      numConstraints()    const;
		//! Estimates the problem complexity.
		/*!
		 * \return sum of c->estimateComplexity(*master()), where c is a problem constraint.
		 */
		uint32      problemComplexity() const;
		//! Returns the size of top-level after last call to endInit()
		size_type   topLevelSize()      const { return lastTopLevel_; }
		//@}

		/*!
		 * \name Learning and distribution
		 * Functions for distributing knowledge.
		 *
		 * \note The functions in this group can be safely called
		 * from multiple threads.
		 */
		 //@{
		 //! Distributes the clause in lits via the distributor.
		 /*!
		  * The function first calls the distribution strategy
		  * to decides whether the clause is a valid candidate for distribution.
		  * If so and a distributor was set, it distributes the clause and returns a handle to the
		  * now shared literals of the clause. Otherwise, it returns 0.
		  *
		  * \param owner The solver that created the clause.
		  * \param lits  The literals of the clause.
		  * \param size  The number of literals in the clause.
		  * \param extra Additional information about the clause.
		  * \note
		  *   If the return value is not null, it is the caller's
		  *   responsibility to release the returned handle (i.e. by calling release()).
		  */
		SharedLiterals* distribute(const Solver& owner, const Literal* lits, uint32 size, const ClauseInfo& extra) const;
		//! Tries to receive at most maxOut clauses.
		/*!
		 * The function queries the distributor object for new clauses to be delivered to
		 * the solver target. Clauses are stored in out.
		 * \return The number of clauses received.
		 */
		uint32          receive(const Solver& target, SharedLiterals** out, uint32 maxOut) const;
		//! Returns the number of learnt binary and ternary clauses
		uint32          numLearntShort() const { return btig_.numLearnt(); }
		//@}
		const Stats& stats()     const { return problem_; }
		uint32       winner()    const { return share_.winner; }
		uint32       numBinary() const { return btig_.numBinary(); }
		uint32       numTernary()const { return btig_.numTernary(); }
		SymbolTable& symTab()    const { return symTabPtr_->symTab; }
		const BTIG&  shortImplications()                     const { return btig_; }
		uint32       numShortImplications(Literal p)         const { return btig_.numEdges(p); }
		void         report(const Event& ev)                 const { if (progress_) progress_->dispatch(ev); }
		bool         report(const Solver& s, const Model& m) const { return !progress_ || progress_->onModel(s, m); }
		void         initStats(Solver& s)                    const;
		void         setWinner(uint32 x) { share_.winner = std::min(x, concurrency()); }
		void         simplifyDB(bool shuffle);
		void         simplify(const Solver& s, Literal p) {
			if (!isShared()) { btig_.removeTrue(s, p); }
		}
		const SolverStats& stats(const Solver& s, bool accu)  const;
	private:
		SharedContext(const SharedContext&);
		SharedContext& operator=(const SharedContext&);
		void init();
		bool unfreezeStep(Solver& s);
		typedef SingleOwnerPtr<Configuration> Config;
		typedef PodVector<VarInfo>::type      VarVec;
		typedef PodVector<SolverStats*>::type StatsVec;
		struct SharedSymTab {
			SharedSymTab() : refs(1) {}
			SymbolTable symTab;
			uint32      refs;
		}*symTabPtr_;     // pointer to shared symbol table
		ProblemStats   problem_;     // problem statistics
		VarVec         varInfo_;     // varInfo[v] stores info about variable v
		BTIG           btig_;        // binary-/ternary implication graph
		Config         config_;      // active configuration
		SolverVec      solvers_;     // solvers associated with this context
		LogPtr         progress_;    // event handler or 0 if not used
		Literal        tag_;         // literal for tagging learnt constraints
		Literal        step_;        // literal for tagging enumeration/step constraints
		size_type      lastTopLevel_;// size of master's top-level after last init
		struct Share {               // Additional data
			uint32 count : 12;        //   max number of objects sharing this object
			uint32 winner : 12;        //   id of solver that terminated the search
			uint32 shareM : 3;        //   physical sharing mode
			uint32 shortM : 1;        //   short clause mode
			uint32 frozen : 1;        //   is adding of problem constraints allowed?
			uint32 seed : 1;        //   set seed of new solvers
			uint32 satPreM : 1;        //   preprocessing mode
			Share() : count(1), winner(0), shareM((uint32)ContextParams::share_auto), shortM(0), frozen(0), seed(0), satPreM(0) {}
		}              share_;
		StatsVec       accu_;        // optional stats accumulator for incremental solving
	};
	//@}
}
#endif
