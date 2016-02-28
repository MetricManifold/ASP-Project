// 
// Copyright (c) 2006-2013, Benjamin Kaufmann
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
#ifndef CLASP_ENUMERATOR_H_INCLUDED
#define CLASP_ENUMERATOR_H_INCLUDED

#ifdef _MSC_VER
#pragma once
#endif
#include "literal.h"
#include "constraint.h"
#include "util/misc_types.h"

namespace Clasp {
	class Solver;
	class SharedContext;
	class Enumerator;
	class EnumerationConstraint;
	class SharedMinimizeData;
	class MinimizeConstraint;

	//! Type for storing a model.
	struct Model {
		enum Type { model_sat = 0, model_cons = 1, max_value = 1 };
		//! True if this model stores current (cautious/brave) consequences.
		bool     consequences()    const { return (type & model_cons) != 0; }
		//! True if this model stores a (total) assignment satisfying the problem.
		bool     values()          const { return !consequences(); }
		//! For sat models, value of v in model. Otherwise, undefined.
		ValueRep value(Var v)      const { assert(v < size); return model[v]; }
		//! True if p is true in model or part of current consequences.
		bool     isTrue(Literal p) const { return value(p.var()) == trueValue(p); }
		uint64                    num;   // running number of this model
		const ValueRep*           model; // variable assignment or consequences
		const SharedMinimizeData* costs; // associated costs (or 0)
		uint32                    size;  // size of model
		uint32                    type;  // type of model
	};

	/**
	 * \defgroup enumerator Enumerators and related classes
	 */
	 //@{

	 //! Interface for supporting enumeration of models.
	 /*!
	  * Enumerators are global w.r.t a solve algorithm. That is,
	  * even if the solve algorithm itself uses a parallel search, there
	  * shall be only one enumerator and it is the user's responsibility
	  * to protect the enumerator with appropriate locking.
	  *
	  * Concrete enumerators must implement a function for preparing a problem for enumeration
	  * and an EnumerationConstraint to be added to each solver attached to the problem.
	  * It is then the job of the actual solve algorithm to commit models to the enumerator
	  * and to integrate new information into attached solvers via appropriate calls to
	  * Enumerator::update().
	  */
	class Enumerator {
	public:
		typedef EnumerationConstraint*       ConPtr;
		typedef const EnumerationConstraint* ConPtrConst;
		typedef const SharedMinimizeData*    Minimizer;
		explicit Enumerator();
		virtual ~Enumerator();

		//! Prepares the problem for enumeration.
		/*!
		 * The function shall be called once before search is started and before SharedContext::endInit()
		 * was called. It freezes enumeration-related variables and adds a suitable enumeration constraint
		 * to the master solver.
		 *
		 * \pre The problem is not yet frozen, i.e. SharedContext::endInit() was not yet called.
		 * \param problem The problem on which this enumerator should work.
		 * \param min     Optional minimization constraint to be applied during enumeration.
		 * \param limit   Optional hint on max number of models to compute.
		 *
		 * \note In the incremental setting, init() must be called once for each incremental step.
		 * \note The enumerator takes ownership of the minimize constraint (if any). That is,
		 *       it does not increase its reference count.
		 */
		int   init(SharedContext& problem, SharedMinimizeData* min = 0, int limit = 0);

		//! Updates the given solver with enumeration-related information.
		/*!
		 * The function is used to integrate enumeration-related information,
		 * like minimization bounds or previously committed models, into the search space of s.
		 * It shall be called once before search is started and once after each commit.
		 *
		 * \param s The solver to update.
		 * \note The function is concurrency-safe, i.e. can be called
		 *       concurrently by different solvers.
		 */
		bool   update(Solver& s)  const;

		//! Commits the model stored in the given solver.
		/*!
		 * Once the model is committed, Enumerator::update(s) shall be called
		 * in order to continue search for further models.
		 *
		 * \pre The solver's assignment is total.
		 *
		 * \note The function is *not* concurrency-safe, i.e. in a parallel search
		 *       at most one solver shall call the function at any one time.
		 */
		Model  commitModel(Solver& s);

		//! Processes an unsatisfiable path stored in the given solver.
		/*!
		 * The return value determines how search should proceed.
		 * If true is returned, the enumerator has relaxed at least one constraint
		 * and search may continue after a call to Enumerator::update();
		 * Otherwise, the search shall be stopped.
		 *
		 * \note The function is *not* concurrency-safe, i.e. in a parallel search
		 *       at most one solver shall call the function at any one time.
		 */
		bool   commitUnsat(Solver& s, bool updateShared);

		//! Removes active (minimization) bound in s (if any).
		void   relax(Solver& s) const;

		//! Returns the number of model enumerated so far.
		uint64       enumerated()       const { return model_.num; }
		//! Returns a value > 0 if optimization and > 1 if hierarchical optimization is active.
		int          optimize()         const;
		//! Returns true if enumerator computes consequences.
		bool         consequences()     const { return model_.consequences(); }
		//! Returns the type of models this enumerator computes.
		virtual int  modelType()        const { return Model::model_sat; }
		//! Returns whether or not this enumerator supports full restarts once a model was found.
		virtual bool supportsRestarts() const { return true; }
		//! Returns whether or not this enumerator supports parallel search.
		virtual bool supportsParallel() const { return true; }
		//! Returns whether this enumerator requires exhaustive search to produce a definite answer.
		/*!
		 * The default implementation returns optimize() || consequences().
		 */
		virtual bool exhaustive()       const { return optimize() != 0 || consequences(); }
		//! Returns true if the given solver currently stores an uncommitted model.
		bool         hasModel(const Solver& s)      const;
		//! Returns true if the given solver currently has an active minimization bound.
		bool         hasBound(const Solver& s)      const;
		//! Sets whether the search path stored in s is disjoint from all others.
		void         setDisjoint(Solver& s, bool b) const;
		Minimizer    minimizer()                    const { return mini_; }
	protected:
		//! Shall prepare the enumerator and freeze any enumeration-related variable.
		/*!
		 * \return A prototypical enumeration constraint to be used in the master solver.
		 */
		virtual ConPtr doInit(SharedContext& ctx, MinimizeConstraint* m, int numModels) = 0;
		ConPtr         constraint(const Solver& s) const;
	private:
		Enumerator(const Enumerator&);
		Enumerator& operator=(const Enumerator&);
		SharedMinimizeData* mini_;
		Model               model_;
	};

	//! A solver-local (i.e. thread-local) constraint to support enumeration.
	/*!
	 * An enumeration constraint is used to extract/store enumeration-related information
	 * from models.
	 */
	class EnumerationConstraint : public Constraint {
	public:
		typedef EnumerationConstraint* ConPtr;
		//! Returns true if the constraint currently stores an uncommitted model.
		bool   hasModel()    const { return (stateOpts_ & flag_has_model) != 0u; }
		//! Returns true if the current model is symmetric to some already committed model.
		bool   isSymmetric() const { return (stateOpts_ & flag_is_sym) != 0u; }
		//! Returns true if the current model was committed to an enumerator.
		bool   isCommitted() const { return (stateOpts_ & state_committed) != 0u; }
		//! Returns true if optimization is active.
		bool   optimize()    const;
		//! Returns true if a (minimization) bound is active.
		bool   hasBound()    const;

		// Methods used by enumerator
		bool update(Solver& s);
		void setIgnoreSymmetric(bool x);
		void setDisjoint(bool x);
		void relax(const Solver& s);
		void commitModel(Enumerator& ctx, Solver& s);
		bool nextModel(Solver& s);
	protected:
		EnumerationConstraint(Solver& s, MinimizeConstraint* min);
		virtual ~EnumerationConstraint();
		// base interface
		virtual void        destroy(Solver* s, bool detach);
		virtual PropResult  propagate(Solver&, Literal, uint32&) { return PropResult(true, true); }
		virtual void        reason(Solver&, Literal, LitVec&) {}
		// own interface
		virtual bool        doUpdate(Solver& s, bool disjoint) = 0;
		virtual void        doCommitModel(Enumerator&, Solver&) {}
		bool                integrateBound(Solver& s) const;
		MinimizeConstraint* cloneMinimizer(Solver& s) const;
	private:
		enum State { state_open = 0u, state_committed = 1u };
		enum Flag { flag_is_sym = 4u, flag_has_model = 8u, flag_no_sym = 16u, flag_disjoint = 32u };
		enum { clear_model_mask = ~uint32(state_committed | flag_is_sym | flag_has_model) };
		MinimizeConstraint* mini_;
		LitVec              sym_;
		uint32              stateOpts_;
	};
	//@}

#define CLASP_ENUM_OPTIONS(OPTION, SELF) \
	OPTION("enum-mode,e" , enum_mode   , X(defaultsTo("auto")->state(Value::value_defaulted)), "Configure enumeration algorithm [%D]\n" \
		"      %A: {bt|record|brave|cautious|auto}\n" \
		"        bt      : Backtrack decision literals from solutions\n" \
		"        record  : Add nogoods for computed solutions\n" \
		"        brave   : Compute brave consequences (union of models)\n" \
		"        cautious: Compute cautious consequences (intersection of models)\n" \
		"        auto    : Use bt for enumeration and record for optimization", STORE_ENUM(SELF.type, \
		_2("bt", EnumOptions::enum_bt), _2("record", EnumOptions::enum_record), _2("brave", EnumOptions::enum_brave), \
		_2("cautious", EnumOptions::enum_cautious), _2("auto", EnumOptions::enum_auto))) \
	OPTION("project"     , project     , X(implicit("6")), "Project models to named atoms", STORE(SELF.project)) \
	OPTION("number,n"    , number      , X(arg("<n>"))   , "Compute at most %A models (0 for all)\n", STORE(SELF.numModels)) \
	OPTION("opt-ignore"  , opt_ignore  , X(flag())       , "Ignore optimize statements", {bool b; return store(value, b) && SELF.setOptMode(b?-1:MinimizeMode_t::opt_def);}) \
	OPTION("opt-sat"     , opt_sat     , X(flag())       , "Treat DIMACS input as MaxSAT optimization problem", STORE(SELF.opt.maxSat)) \
	OPTION("opt-hierarch", opt_hierarch, X(arg("{0..3}")->implicit("1")),  "Process optimize statements in order of priority\n" \
		"    For each criterion use:\n" \
		"      1: fixed step size of one\n" \
		"      2: exponentially increasing step sizes\n" \
		"      3: exponentially decreasing step sizes", { \
		uint32 newMode; \
		return store(value, newMode) && newMode <= 3u && SELF.setOptMode(newMode|MinimizeMode_t::optimize);})\
	OPTION("opt-all"     , opt_all     , X(arg("<opt>...")), "Compute models <= %A", STORE(SELF.optAll())) \
	OPTION("opt-value"   , opt_value   , X(arg("<opt>...")), "Initialize objective function(s)\n", STORE(SELF.optValue()))

//! Options for configuring enumeration.
	struct EnumOptions {
		enum EnumType { enum_auto = 0, enum_bt = 1, enum_record = 2, enum_consequences = 4, enum_brave = 5, enum_cautious = 6 };
		EnumOptions() : numModels(-1), type(enum_auto), project(0) {}
		Enumerator* createEnumerator() const;
		bool        consequences()     const { return (type & enum_consequences) != 0; }
		bool        optimize()         const;
		int       numModels;  /*!< Number of models to compute. */
		EnumType  type;       /*!< Enumeration type to use.     */
		uint32    project;    /*!< Options for projection.      */
		//! Options for configuring optimization.
		struct Optimize {
			Optimize();
			SumVec vals;   /*!< Initial values for optimize statements. */
			int    mode;   /*!< MinimizeMode_t or < 0 to ignore optimize statements. */
			bool   maxSat; /*!< Treat DIMACS input as MaxSat */
			bool   disabled() const { return mode < 0; }
		}         opt;
		static Enumerator* createModelEnumerator(const EnumOptions& opts);
		static Enumerator* createConsEnumerator(const EnumOptions& opts);
		static Enumerator* nullEnumerator();
		bool setOptMode(int mode);
		SumVec& optAll();
		SumVec& optValue();
	};

}

#endif
