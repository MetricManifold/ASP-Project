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
#ifndef CLASP_MINIMIZE_CONSTRAINT_H_INCLUDED
#define CLASP_MINIMIZE_CONSTRAINT_H_INCLUDED

#ifdef _MSC_VER
#pragma warning (disable : 4200) // nonstandard extension used : zero-sized array
#pragma once
#endif

#include "constraint.h"
#include "util/atomic.h"
#include <cassert>

namespace Clasp {
	class MinimizeConstraint;
	//! Supported minimization modes.
	/*!
	 * Defines the possible minimization modes used during solving.
	 * If optimize is used, an optimal solution is one that is
	 * strictly smaller than all previous solutions. Otherwise
	 * an optimal solution is one that is no greater than an initially
	 * given optimum.
	 */
	struct MinimizeMode_t {
		enum Mode {
			enumerate = 0, /**< enumerate w.r.t given optimum */
			optimize = 4, /**< optimize using < */
			opt_def = 0 | optimize, /**< default optimization mode */
			opt_hier = 1 | optimize, /**< basic hierarchical optimization */
			opt_inc = 2 | optimize, /**< hierarchical optimization with increasing steps */
			opt_dec = 3 | optimize, /**< hierarchical optimization with decreasing steps */
		};
	};
	typedef MinimizeMode_t::Mode MinimizeMode;

	//! A type holding data (possibly) shared between a set of minimize constraints.
	/*!
	 * \ingroup shared
	 */
	class SharedMinimizeData {
	public:
		//! A type to represent a weight at a certain level.
		/*!
		 * Objects of this type are used to create sparse vectors of weights. E.g.
		 * a weight vector (w1@L1, w2@L3, w3@L5) is represented as [<L1,1,w1><L3,1,w2><L5,0,w3>],
		 * where each <level, next, weight>-tuple is an object of type LevelWeight.
		 */
		struct LevelWeight {
			LevelWeight(uint32 l, weight_t w) : level(l), next(0), weight(w) {}
			uint32   level : 31; /**< The level of this weight. */
			uint32   next : 1; /**< Does this weight belong to a sparse vector of weights? */
			weight_t weight;     /**< The weight at this level. */
		};
		//! A type for holding sparse vectors of level weights of a multi-level constraint.
		typedef PodVector<LevelWeight>::type WeightVec;

		struct Step {
			Step(uint32 l = 0, wsum_t s = 0) : gen(0), level(l), step(s) { }
			uint32 gen;   // generation to which the step applies
			uint32 level; // active level
			wsum_t step;  // active step
		};
		explicit SharedMinimizeData(const SumVec& lhsAdjust, MinimizeMode m = MinimizeMode_t::optimize);
		SharedMinimizeData* share() { ++count_; return this; }
		void                release() { if (--count_ == 0) destroy(); }
		//! Number of minimize statements contained in this constraint.
		uint32         numRules()      const { return static_cast<uint32>(adjust_.size()); }
		uint32         maxLevel()      const { return numRules() - 1; }
		static wsum_t  maxBound() { return INT64_MAX; }
		//! Returns the active minimization mode.
		MinimizeMode   mode()          const { return mode_; }
		//! Hierarchical optimization enabled?
		bool           hierarchical()  const { return mode_ > MinimizeMode_t::optimize; }
		//! Returns the lower bound of level x.
		wsum_t         lower(uint32 x) const { return lower_[x]; }
		//! Returns the upper bound of level x.
		wsum_t         upper(uint32 x) const { return opt_[active()][x]; }
		//! Returns the adjustment for level x.
		wsum_t         adjust(uint32 x)const { return adjust_[x]; }
		//! Returns the current (ajusted) optimum for level x.
		wsum_t         optimum(uint32 x)const { return upper(x) + adjust(x); }
		//! Returns the level of the literal with the given index i.
		uint32         level(uint32 i) const { return numRules() == 1 ? 0 : weights[lits[i].second].level; }
		uint32         generation()    const { return gCount_; }
		Step           step()          const { return step_; }
		//! Assumes literals from last model to false until either a conflict is reached or all literals are assigned.
		bool modelHeuristic(Solver& s) const;

		//! Sets optOut to (opt - (applyStep * opt->step)).
		bool next(bool applyStep, wsum_t* optOut) const;
		/*!
		 * \name interface for optimization
		 * The following functions shall not be called concurrently.
		 */
		 //@{

		 //! Sets the enumeration mode and forces computation of models <= bound.
		 /*!
		  * \note If m is MinimizeMode::enumerate, the caller should always
		  * set a bound. Otherwise, *all* solutions are considered optimal.
		  */
		bool setMode(MinimizeMode m, const wsum_t* bound = 0, uint32 boundSize = 0);
		bool setMode(MinimizeMode m, const SumVec& bound) { return setMode(m, bound.empty() ? 0 : &bound[0], (uint32)bound.size()); }
		void resetBounds();

		//! Attaches a new minimize constraint to this data object.
		/*!
		 * If addRef is true, the ref count of the shared object
		 * is increased. Otherwise, the new minimize constraint
		 * inherits the reference to the shared object.
		 */
		MinimizeConstraint* attach(Solver& s, bool addRef = true);

		//! Makes opt the new optimum.
		/*!
		 * \pre opt is a pointer to an array of size numRules()
		 */
		const SumVec* setOptimum(const wsum_t* opt);

		//! Sets the next level to optimize.
		/*!
		 * \return
		 *   - true if there is a next level to optimize
		 *   - false otherwise
		 */
		bool optimizeNext() { return optimizeNext(active()); }
		//@}
	private:
		typedef Clasp::atomic<uint32> Atomic;
		SumVec       adjust_;  // initial bound adjustments
		SumVec       lower_;   // (unadjusted) lower bound of constraint
		SumVec       opt_[2];  // buffers for update via "double buffering"
		Step         step_;    // next bound to check (opt - step)
		MinimizeMode mode_;    // how to compare assignments?
		Atomic       count_;   // number of refs to this object
		Atomic       gCount_;  // generation count - used when updating optimum
	public:
		WeightVec       weights;  // sparse vectors of weights - only used for multi-level constraints
		WeightLiteral   lits[0];  // (shared) literals - terminated with posLit(0)
	private:
		~SharedMinimizeData();
		void destroy() const;
		SharedMinimizeData(const SharedMinimizeData&);
		SharedMinimizeData& operator=(const SharedMinimizeData&);
		void setStep(uint32 n);
		bool optimizeNext(uint32 n);
		uint32 active() const { return gCount_ & 1; }
	};

	//! Helper class for creating minimize constraints.
	class MinimizeBuilder {
	public:
		typedef SharedMinimizeData SharedData;
		MinimizeBuilder();
		~MinimizeBuilder();

		bool             hasRules() const { return !adjust_.empty(); }
		uint32           numRules() const { return (uint32)adjust_.size(); }
		uint32           numLits()  const { return (uint32)lits_.size(); }
		//! Adds a minimize statement.
		/*!
		 * \param lits the literals of the minimize statement
		 * \param adjustSum the initial sum of the minimize statement
		 */
		MinimizeBuilder& addRule(const WeightLitVec& lits, wsum_t adjustSum = 0);
		MinimizeBuilder& addLit(uint32 lev, WeightLiteral lit);

		//! Creates a new data object from previously added minimize statements.
		/*!
		 * The function creates a new minimize data object from
		 * the previously added minimize statements. The returned
		 * object can be used to attach one or more MinimizeConstraints.
		 * \param ctx A ctx object used to simplify minimize statements.
		 * \return a new data object representing previously added minimize statements
		 *  or 0 if minimize statements are initially inconsistent!
		 */
		SharedData*      build(SharedContext& ctx);
		void clear();
	private:
		struct Weight {
			Weight(uint32 lev, weight_t w) : level(lev), weight(w), next(0) {}
			uint32   level;
			weight_t weight;
			Weight*  next;
			static void free(Weight*& w);
		};
		typedef std::pair<Literal, Weight*> LitRep;
		typedef PodVector<LitRep>::type     LitRepVec;
		struct CmpByLit {
			bool operator()(const LitRep& lhs, const LitRep& rhs) const;
		};
		struct CmpByWeight {
			bool operator()(const LitRep& lhs, const LitRep& rhs) const;
			int  compare(const LitRep& lhs, const LitRep& rhs) const;
		};
		void     unfreeze();
		bool     prepare(SharedContext& ctx);
		void     addTo(LitRep l, SumVec& vec);
		void     mergeReduceWeight(LitRep& x, LitRep& by);
		weight_t addFlattened(SharedData::WeightVec& x, const Weight& w);
		bool     eqWeight(const SharedData::LevelWeight* lhs, const Weight& rhs);
		weight_t addLitImpl(uint32 lev, WeightLiteral lit) {
			if (lit.second > 0) { lits_.push_back(LitRep(lit.first, new Weight(lev, lit.second)));   return 0; }
			if (lit.second < 0) { lits_.push_back(LitRep(~lit.first, new Weight(lev, -lit.second))); return lit.second; }
			return 0;
		}
		LitRepVec lits_;  // all literals
		SumVec    adjust_;// lhs adjustments
		bool      ready_; // prepare was called
	};

	//! Implements a (meta-)constraint for supporting Smodels-like minimize statements.
	/*!
	 * \ingroup constraint
	 * A solver contains at most one minimize constraint, but a minimize constraint
	 * may represent several minimize statements. In that case, each minimize statement
	 * has a unique level (starting at 0) and minimize statements with a lower level
	 * have higher priority. Priorities are handled like in smodels, i.e. statements
	 * with lower priority become relevant only if all statements with higher priority
	 * currently have an optimal assignment.
	 *
	 * MinimizeConstraint supports two modes of operation: if mode is set to
	 * optimize, solutions are considered optimal only if they are strictly smaller
	 * than previous solutions. Otherwise, if mode is set to enumerate a
	 * solution is valid if it is not greater than the initially set optimum.
	 * Example:
	 *  m0: {a, b}
	 *  m1: {c, d}
	 *  All models: {a, c,...}, {a, d,...} {b, c,...}, {b, d,...} {a, b,...}
	 *  Mode = optimize: {a, c, ...} (m0 = 1, m1 = 1}
	 *  Mode = enumerate and initial opt=1,1: {a, c, ...}, {a, d,...}, {b, c,...}, {b, d,...}
	 *
	 */
	class MinimizeConstraint : public Constraint {
	public:
		friend class SharedMinimizeData;
		typedef SharedMinimizeData       SharedData;
		typedef PodVector<wsum_t>::type  SumVec;

		//! Attaches this object to the given solver.
		bool attach(Solver& s, Literal tag);

		//! Number of minimize statements contained in this constraint.
		uint32 numRules() const { return shared()->numRules(); }

		//! Returns true if this constraint currently has a bound, i.e. is active.
		bool hasBound() const { return *opt_ != SharedData::maxBound(); }

		//! Removes the local upper bound of this constraint and therefore disables it.
		bool relaxBound(const Solver& s);

		//! Sets the current local sum as the global optimum (upper bound).
		/*!
		 * commitBound() shall be called whenever the solver finds a model.
		 * The current local sum is recorded as new optimum in the shared data object.
		 * Once the local bound is committed, the function integrateBound() has to be
		 * called in order to continue optimization.
		 */
		const SumVec* commitBound(const Solver& s) const;

		//! Tries to integrate the next tentative bound into this constraint.
		/*!
		 * Starting from the current optimum stored in the shared data object,
		 * the function tries to integrate the next candidate bound into
		 * this constraint.
		 *
		 * \return The function returns true if integration succeeded. Otherwise
		 * false is returned and s.hasConflict() is true.
		 *
		 * \note If integrateBound() failed, the bound of this constraint
		 *       is relaxed. The caller has to resolve the conflict first
		 *       and then integrateBound() shall be called again.
		 *
		 * \note The caller has to call s.propagate() to propagate any new information
		 *       from the new bound.
		 *
		 * \note If the tag literal (if any) is not true, i.e. the minimize constraint is
		 *       not enabled, or if enumeration mode is active (instead of optimization),
		 *       the function is a noop.
		 */
		bool integrateBound(Solver& s);

		const SharedData* shared() const { return shared_; }

		// base interface
		PropResult  propagate(Solver& s, Literal p, uint32& data);
		void        undoLevel(Solver& s);
		void        reason(Solver& s, Literal p, LitVec& lits);
		bool        minimize(Solver& s, Literal p, CCMinRecursive* r);
		Constraint* cloneAttach(Solver& other);
		void        destroy(Solver*, bool);

		// FOR TESTING ONLY!
		wsum_t sum(uint32 i, bool adjust) const { return sum_[i] + (adjust ? shared_->adjust(i) : 0); }
	protected:
		MinimizeConstraint(SharedData* d);
		~MinimizeConstraint();
		enum PropMode { propagate_new_sum, propagate_new_opt };
		union UndoInfo {
			UndoInfo() : rep(0) {}
			struct {
				uint32 idx : 30; // index of literal on stack
				uint32 newDL : 1; // first literal of new DL?
				uint32 idxSeen : 1; // literal with idx already propagated?
			}      data;
			uint32 rep;
			uint32 index()      const { return data.idx; }
			bool   newDL()      const { return data.newDL != 0u; }
		};
		typedef const WeightLiteral*         Iter;
		uint32    lastUndoLevel(const Solver& s) const;
		void      pushUndo(Solver& s, uint32 litIdx);
		bool      litSeen(uint32 i) const { return undo_[i].data.idxSeen != 0; }
		uint32    updateOpt(const Solver& s, bool applyStep);
		bool      propagateImpl(Solver& s, PropMode m);
		uint32    computeImplicationSet(const Solver& s, const WeightLiteral& it, uint32& undoPos);
		uint32&   active(wsum_t* s) { return active_[s == temp_]; }
		bool      greater(wsum_t* lhs, wsum_t* rhs, uint32 len) const {
			while (*lhs == *rhs && --len) { ++lhs, ++rhs; }
			return *lhs > *rhs;
		}
		// Arithmetic operations
		enum   ArithType { SINGLE_LEVEL_ARITH = 0, MULTI_LEVEL_ARITH = 1 };
		void   assign(wsum_t* lhs, wsum_t* rhs);
		void   add(weight_t wOrIdx);
		void   subtract(wsum_t* lhs, weight_t wOrIdx);
		bool   implied(wsum_t* lhs, weight_t wOrIdx);
		uint32 convert(wsum_t*) { return numRules(); }
		const  wsum_t* sumToOpt() const { return sum_; }

		wsum_t*     sum_;       // current sum
		wsum_t*     opt_;       // current (local) optimum
		wsum_t*     temp_;      // temporary sum; used in propagateImpl() to compute implication level
		SharedData* shared_;    // pointer to (read-only) shared data
		Iter        pos_;       // position of literal to look at next
		UndoInfo*   undo_;      // one "seen" flag for each literal +
		Literal     tag_;       // literal for tagging reasons
		uint32      undoTop_;   // undo stack holding assigned literals
		uint32      posTop_;    // stack of saved "look at" positions
		uint32      active_[2]; // first level to look at (one for sum_ and one for temp_)
		const ArithType type_;  // type of arithmetic operations
	};
} // end namespace Clasp

#endif
