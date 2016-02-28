// 
// Copyright (c) 2006-2012, Benjamin Kaufmann
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
#include "../../libclasp/clasp/enumerator.h"
#include "../../libclasp/clasp/minimize_constraint.h"
#include "../../libclasp/clasp/solver.h"
namespace Clasp {

	/////////////////////////////////////////////////////////////////////////////////////////
	// EnumerationConstraint
	/////////////////////////////////////////////////////////////////////////////////////////
	EnumerationConstraint::EnumerationConstraint(Solver& s, MinimizeConstraint* min) : mini_(min), stateOpts_(0) {
		setDisjoint(optimize() || s.sharedContext()->concurrency() == 1);
		if (optimize()) { setIgnoreSymmetric(true); }
	}
	MinimizeConstraint* EnumerationConstraint::cloneMinimizer(Solver& s) const {
		return mini_ ? static_cast<MinimizeConstraint*>(mini_->cloneAttach(s)) : 0;
	}
	EnumerationConstraint::~EnumerationConstraint() { }
	void EnumerationConstraint::destroy(Solver* s, bool x) { if (mini_) { mini_->destroy(s, x); mini_ = 0; } Constraint::destroy(s, x); }
	bool EnumerationConstraint::integrateBound(Solver& s) const { return !mini_ || (s.isTrue(s.sharedContext()->tagLiteral()) && mini_->integrateBound(s)); }
	bool EnumerationConstraint::optimize() const { return mini_ && mini_->shared()->mode() >= MinimizeMode_t::optimize; }
	bool EnumerationConstraint::hasBound() const { return mini_ && mini_->hasBound(); }
	void EnumerationConstraint::setIgnoreSymmetric(bool x) {
		if (x) { stateOpts_ |= uint32(flag_no_sym); }
		else { stateOpts_ &= ~uint32(flag_no_sym); }
	}
	void EnumerationConstraint::setDisjoint(bool x) {
		if (x || optimize()) { stateOpts_ |= uint32(flag_disjoint); }
		else { stateOpts_ &= ~uint32(flag_disjoint); }
	}
	void EnumerationConstraint::relax(const Solver& s) {
		if (optimize()) { mini_->relaxBound(s); }
		stateOpts_ &= uint32(clear_model_mask);
	}
	bool EnumerationConstraint::update(Solver& s) {
		bool disjoint = (stateOpts_ & flag_disjoint) != 0;
		bool ok = doUpdate(s, disjoint) && integrateBound(s);
		if (s.numFreeVars() != 0 || s.hasConflict() || s.queueSize()) {
			stateOpts_ &= uint32(clear_model_mask);
		}
		return ok || !s.hasConflict();
	}
	void EnumerationConstraint::commitModel(Enumerator& ctx, Solver& s) {
		assert(!isCommitted() || !sym_.empty());
		if (isCommitted()) { nextModel(s); }
		if (!isSymmetric()) {
			sym_.clear();
			if (mini_) { mini_->commitBound(s); }
			if ((stateOpts_ & flag_no_sym) == 0) { sym_ = s.symmetric(); }
			doCommitModel(ctx, s);
		}
		stateOpts_ &= ~uint32(flag_has_model);
		stateOpts_ |= uint32(state_committed) | uint32(sym_.empty() ? 0 : flag_has_model);
	}
	bool EnumerationConstraint::nextModel(Solver& s) {
		bool hasSym = hasModel() && !sym_.empty();
		stateOpts_ &= uint32(clear_model_mask);
		if (!hasSym) { return false; }
		s.satPrepro()->extendModel(s.model, sym_);
		stateOpts_ |= uint32(flag_is_sym | flag_has_model);
		return true;
	}
	/////////////////////////////////////////////////////////////////////////////////////////
	// Enumerator
	/////////////////////////////////////////////////////////////////////////////////////////
	Enumerator::Enumerator() : mini_(0) {}
	Enumerator::~Enumerator() { if (mini_) mini_->release(); }
	int  Enumerator::optimize()        const { return mini_ ? mini_->mode() : 0; }
	bool Enumerator::hasModel(const Solver& s)     const { return constraint(s)->hasModel(); }
	bool Enumerator::hasBound(const Solver& s)     const { return constraint(s)->hasBound(); }
	void Enumerator::setDisjoint(Solver& s, bool b)const { constraint(s)->setDisjoint(b); }
	void Enumerator::relax(Solver& s)              const { constraint(s)->relax(s); }
	int  Enumerator::init(SharedContext& ctx, SharedMinimizeData* min, int limit) {
		typedef MinimizeConstraint* MinPtr;
		ctx.master()->setEnumerationConstraint(0);
		if (mini_ && mini_ != min) { mini_->release(); }
		model_.model = 0;
		model_.costs = min;
		model_.num = 0;
		model_.size = 0;
		model_.type = uint32(modelType());
		mini_ = min;
		MinPtr mc = mini_ ? mini_->attach(*ctx.master()) : 0;
		limit = limit >= 0 ? limit : 1 - int(exhaustive());
		if (limit != 1) { ctx.preserveModels(); }
		ConPtr c = doInit(ctx, mc, limit);
		ctx.master()->setEnumerationConstraint(c);
		if (limit && exhaustive()) {
			if (optimize()) { ctx.report(warning(Event::subsystem_prepare, "#models not 0: optimality of last model not guaranteed.")); }
			if (consequences()) { ctx.report(warning(Event::subsystem_prepare, "#models not 0: last model may not cover consequences.")); }
		}
		if (consequences() && optimize()) {
			ctx.report(warning(Event::subsystem_prepare, "Optimization: Consequences may depend on enumeration order."));
		}
		return limit;
	}
	Enumerator::ConPtr Enumerator::constraint(const Solver& s) const {
		return static_cast<ConPtr>(s.enumerationConstraint());
	}
	Model Enumerator::commitModel(Solver& s) {
		assert(s.numFreeVars() == 0 && !s.hasConflict() && s.queueSize() == 0 && constraint(s));
		ConPtr c = constraint(s);
		s.stats.addModel(s.decisionLevel());
		c->commitModel(*this, s);
		++model_.num;
		model_.model = s.model.size() ? &s.model[0] : 0;
		model_.size = (uint32)s.model.size();
		return model_;
	}
	bool Enumerator::commitUnsat(Solver& s, bool updateShared) {
		relax(s);
		uint32 tagDL = s.isTrue(s.sharedContext()->tagLiteral()) ? s.level(s.sharedContext()->tagLiteral().var()) : 0;
		if (tagDL && tagDL >= s.rootLevel()) {
			s.setBacktrackLevel(0);
			s.popRootLevel((tagDL - s.rootLevel()) + 1);
		}
		// Currently we only support "global" relaxation during hierarchical optimization
		return updateShared && mini_ && mini_->hierarchical() && mini_->optimizeNext();
	}

	bool Enumerator::update(Solver& s) const {
		assert(!s.hasConflict());
		ConPtr c = constraint(s);
		bool model = c->isCommitted();
		if (model) {
			if (c->nextModel(s)) { return true; }
			if (s.strategy.restartOnModel) { s.undoUntil(0); }
			if (optimize()) { s.strengthenConditional(); }
			for (;;) {
				if (c->update(s)) {
					return !optimize() || mini_->modelHeuristic(s) || s.resolveConflict();
				}
				if (!s.hasConflict() || !s.resolveConflict()) { return false; }
			}
		}
		return c->update(s);
	}
	/////////////////////////////////////////////////////////////////////////////////////////
	// EnumOptions
	/////////////////////////////////////////////////////////////////////////////////////////
	bool EnumOptions::optimize() const { return opt.mode >= MinimizeMode_t::optimize; }
	EnumOptions::Optimize::Optimize() : mode(MinimizeMode_t::optimize), maxSat(false) {}
	Enumerator* EnumOptions::createEnumerator() const {
		if (consequences()) { return createConsEnumerator(*this); }
		else if (type <= enum_record) { return createModelEnumerator(*this); }
		else { return 0; }
	}

	bool EnumOptions::setOptMode(int m) {
		if (opt.mode != m) {
			if (opt.mode < 0 || (m < 0 && !opt.vals.empty())) { throw std::logic_error("'opt-ignore': other optimize options are disabled!"); }
			if ((opt.mode < 1 && m > 0) || (opt.mode > MinimizeMode_t::opt_def && m < opt.mode)) { throw std::logic_error("Mutually exclusive optimization modes!"); }
		}
		opt.mode = m;
		return true;
	}

	SumVec& EnumOptions::optAll() {
		if (!opt.vals.empty() && opt.mode > MinimizeMode_t::enumerate) { throw std::logic_error("'opt-all' and 'opt-value' are mutually exclusive!"); }
		setOptMode(MinimizeMode_t::enumerate);
		return opt.vals;
	}
	SumVec& EnumOptions::optValue() {
		if (!opt.vals.empty() && opt.mode == MinimizeMode_t::enumerate) { throw std::logic_error("'opt-all' and 'opt-value' are mutually exclusive!"); }
		setOptMode(std::max(opt.mode, (int)MinimizeMode_t::optimize));
		return opt.vals;
	}

	Enumerator* EnumOptions::nullEnumerator() {
		struct NullEnum : Enumerator {
			ConPtr doInit(SharedContext& ctx, MinimizeConstraint* m, int) {
				struct Constraint : public EnumerationConstraint {
					Constraint(Solver& s, MinimizeConstraint* min) : EnumerationConstraint(s, min) {}
					bool        doUpdate(Solver& s, bool) { s.setStopConflict(); return false; }
					Constraint* cloneAttach(Solver& s) { return new Constraint(s, cloneMinimizer(s)); }
				};
				return new Constraint(*ctx.master(), m);
			}
		};
		return new NullEnum;
	}


}
