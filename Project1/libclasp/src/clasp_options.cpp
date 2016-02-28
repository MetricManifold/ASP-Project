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
#include "../clasp/cli/clasp_options.h"
#include "../clasp/minimize_constraint.h"
#include "../clasp/lookahead.h"
#include "../clasp/unfounded_check.h"
#include "../../libprogram_opts/program_opts/program_options.h"
#include "../../libprogram_opts/program_opts/typed_value.h"
#include <cstring>
#include <cstdarg>
#include <cfloat>
#include <fstream>
/////////////////////////////////////////////////////////////////////////////////////////
// Functions for parsing certain clasp types
/////////////////////////////////////////////////////////////////////////////////////////
namespace bk_lib {
	template <class T>
	static int xconvert(const char* x, pod_vector<T>& out, const char** errPos, int) {
		using bk_lib::xconvert;
		const char* n = x;
		std::size_t s = out.size();
		for (T temp; xconvert(n, temp, &n, 0); ++n) {
			out.push_back(temp);
			if (*n != ',') { break; }
		}
		if (errPos) { *errPos = n; }
		return static_cast<int>(out.size() - s);
	}
}
namespace Clasp {
	static int xconvert(const char* x, ScheduleStrategy& out, const char** errPos, int e) {
		using bk_lib::xconvert;
		if (!x) { return 0; }
		const char* next = std::strchr(x, ',');
		uint32      base = 0;
		int         tok = 1;
		if (errPos) { *errPos = x; }
		if (!next || !xconvert(next + 1, base, &next, e) || base == 0) { return 0; }
		if (strncasecmp(x, "f,", 2) == 0 || strncasecmp(x, "fixed,", 6) == 0) {
			out = ScheduleStrategy::fixed(base);
		}
		else if (strncasecmp(x, "l,", 2) == 0 || strncasecmp(x, "luby,", 5) == 0) {
			uint32 lim = 0;
			if (*next == ',' && !xconvert(next + 1, lim, &next, e)) { return 0; }
			out = ScheduleStrategy::luby(base, lim);
		}
		else if (strncmp(x, "+,", 2) == 0 || strncasecmp(x, "add,", 4) == 0) {
			std::pair<uint32, uint32> arg(0, 0);
			if (*next != ',' || !xconvert(next + 1, arg, &next, e)) { return 0; }
			out = ScheduleStrategy::arith(base, arg.first, arg.second);
		}
		else if (strncmp(x, "x,", 2) == 0 || strncmp(x, "*,", 2) == 0 || strncasecmp(x, "d,", 2) == 0) {
			std::pair<double, uint32> arg(0, 0);
			if (*next != ',' || !xconvert(next + 1, arg, &next, e)) { return 0; }
			if (strncasecmp(x, "d", 1) == 0 && arg.first > 0.0) { out = ScheduleStrategy(ScheduleStrategy::user_schedule, base, arg.first, arg.second); }
			else if (strncasecmp(x, "d", 1) != 0 && arg.first >= 1.0) { out = ScheduleStrategy::geom(base, arg.first, arg.second); }
			else { return 0; }
		}
		else { next = x; tok = 0; }
		if (errPos) { *errPos = next; }
		return tok;
	}
	namespace Cli {
		template <class It>
		static bool nextArg(It& it, It end, std::string& out) {
			out.clear();
			while (it != end && *it == ' ') { ++it; } // skip leading white
			while (it != end && *it != ' ') {
				if (*it != '\'' && *it != '"') { out += *it++; continue; }
				for (char c = *it++, x = 0; it != end && (x = *(it++)) != c;) { out += x; }
			}
			return !out.empty();
		}
		/////////////////////////////////////////////////////////////////////////////////////////
		// DEFAULT ACTIONS
		/////////////////////////////////////////////////////////////////////////////////////////
		template <class T>
		bool store(const char* value, T& out) {
			return bk_lib::string_cast(value, out);
		}
		static bool findInValueListImpl(const char* value, int& out, const char* k1, int v1, va_list args) {
			if (strcasecmp(value, k1) == 0) { out = v1; return true; }
			while (const char* key = va_arg(args, const char *)) {
				int val = va_arg(args, int);
				if (strcasecmp(value, key) == 0) { out = val; return true; }
			}
			return false;
		}

		template <class T>
		static bool findInValueList(const char* value, T& out, const char* k1, int v1, ...) {
			va_list args;
			va_start(args, v1);
			int temp;
			bool found;
			if ((found = findInValueListImpl(value, temp, k1, v1, args)) == true) {
				out = static_cast<T>(temp);
			}
			va_end(args);
			return found;
		}
		bool isDisabled(const char* value) {
			return value && (std::strcmp(value, "0") == 0 || std::strcmp(value, "no") == 0 || std::strcmp(value, "off") == 0 || std::strcmp(value, "false") == 0);
		}
#define X_SET(x, v)           ( ((x)=(v)) == (v) )
#define X_SET_LEQ(x, v, m)    ( ((v)<=(m)) && X_SET((x), (v)) )
#define X_SET_OR_FILL(x, v)   ( X_SET((x),(v)) || ((x) = 0, (x) = ~(x),true) ) 
#define X_SET_OR_ZERO(x,v)    ( X_SET((x),(v)) || X_SET((x),uint32(0)) )
#define X_SET_R(x, v, lo, hi) ( ((lo)<=(v)) && ((v)<=(hi)) && X_SET((x), (v)) )
#define X_STORE_ENUM_U(x, v, X, ...) ( findInValueList((v), temp, X, ##__VA_ARGS__, _2(0,0)) && X_SET(x, temp) )
#define X_STORE_ENUM(x, v, X, ...)   ( findInValueList((v), x, X, ##__VA_ARGS__, _2(0,0)) )
#define STORE(x)                return Clasp::Cli::store(value, x)
#define NOTIFY(S,key)           return Clasp::Cli::set_opt_##key(S, value)
#define STORE_ENUM(x, X, ...)   return X_STORE_ENUM(x, value, X, ##__VA_ARGS__)
#define STORE_ENUM_U(x, X, ...) return X_STORE_ENUM_U(x, value, X, ##__VA_ARGS__)
#define SET_LEQ(x, y)           STORE(temp) && X_SET_LEQ(x, temp, y)
#define SET_OR_ZERO(x)          STORE(temp) && X_SET_OR_ZERO(x, temp)
#define SET_OR_FILL(x)          STORE(temp) && X_SET_OR_FILL(x, temp)
#define _2(x, y) static_cast<const char*>(x), static_cast<int>(y)
#define PAIR(T, U) std::pair<T,U>
#define INIT_LIST(...) { __VA_ARGS__ }
		typedef std::pair<uint32, uint32> UPair;
		/////////////////////////////////////////////////////////////////////////////////////////
		// Setting option values - helpers
		/////////////////////////////////////////////////////////////////////////////////////////
#define CONFIG_OPTIONS(OPTION, SELF) \
OPTION("configuration", configuration, X(defaultsTo("frumpy")->state(Value::value_defaulted)), "Configure default configuration [%D]\n" \
	"      %A: {frumpy|jumpy|handy|crafty|trendy}\n"                    \
	"        frumpy: Use conservative defaults\n"                       \
	"        jumpy : Use aggressive defaults\n"                         \
	"        handy : Use defaults geared towards large problems\n"      \
	"        crafty: Use defaults geared towards crafted problems\n"    \
	"        trendy: Use defaults geared towards industrial problems", STORE_ENUM_U(SELF.active()->cliDefCfg, \
	_2("frumpy", config_frumpy), _2("jumpy",  config_jumpy), \
	_2("handy" , config_handy) , _2("crafty", config_crafty),\
	_2("trendy", config_trendy))) \
OPTION("tester" , tester, _0, "Pass (quoted) string of <options> to tester", STORE(SELF.testerCmd_)) \
OPTION("portfolio!,p", portfolio, _0, "Use portfolio to configure solver(s)\n"\
	"      %A: {default|seed|<file>}", NOTIFY(SELF.active()->cliPortKey,portfolio))

		static bool set_opt_portfolio(uint8& port, const char* value) {
			if (std::strcmp(value, "default") == 0) { return X_SET(port, unsigned(ClaspCliConfig::portfolio_def)); }
			else if (std::strcmp(value, "no") == 0) { return X_SET(port, unsigned(ClaspCliConfig::portfolio_no)); }
			else if (std::strcmp(value, "seed") == 0) { return X_SET(port, unsigned(ClaspCliConfig::portfolio_seed)); }
			else { port = (uint8)ClaspCliConfig::loadPortfolio(value); return true; }
		}
		class ClaspCliConfig::ProgOption : public ProgramOptions::Value {
		public:
			ProgOption(ClaspCliConfig& c, int o) : ProgramOptions::Value(0), config_(&c), option_(o) {}
			bool doParse(const std::string&, const std::string& value) {
				return option_ >= 0 ? config_->set(static_cast<Clasp::Cli::Option>(option_), value.c_str()) : config_->set(static_cast<ConfigOption>(option_), value.c_str());
			}
			int option() const { return option_; }
		private:
			ClaspCliConfig* config_;
			int             option_;
		};
		/////////////////////////////////////////////////////////////////////////////////////////
		// ClaspCliConfig
		/////////////////////////////////////////////////////////////////////////////////////////
		ClaspCliConfig::ClaspCliConfig() { }
		ClaspCliConfig::ScopedSet::ScopedSet(ClaspCliConfig& s, uint8 mode, uint32 sId) : self(&s) {
			mode |= static_cast<uint8>(sId << 2);
			if (sId) { mode |= mode_port; }
			s.cliTemp = mode;
		}
		ClaspCliConfig::ScopedSet::~ScopedSet() { self->cliTemp = 0; }
		ClaspCliConfig::ProgOption* ClaspCliConfig::createOption(int o) {
			return new ProgOption(*this, o);
		}
		void ClaspCliConfig::init(uint32 solverId, DefaultConfig generator, DefaultConfig tester) {
			ProgramOptions::ParsedOptions exclude;
			if (generator != config_default) {
				addSolver(solverId) = SolverParams();
				addSearch(solverId) = SolveParams();
				setDefaults(this, solverId, generator, exclude);
			}
			if (tester != config_default) {
				setDefaults(addTesterConfig(), solverId, tester, exclude);
			}
		}
		void ClaspCliConfig::init(ProgramOptions::OptionContext& root) {
#define CLASP_ADD_OPTION(n, k, a, d, x)   (n,createOption(opt_##k)a, d)	
#define CLASP_ADD(OPTION_TABLE) OPTION_TABLE(CLASP_ADD_OPTION,)
			using namespace ProgramOptions;
#define X(a) ->a
#define _0
			OptionGroup configOpts("Clasp.Config Options");
			configOpts.addOptions()
				CLASP_ADD(CONFIG_OPTIONS)
				CLASP_ADD(CLASP_CONTEXT_OPTIONS)
				;
			root.add(configOpts);
			OptionGroup solving("Clasp.Solving Options");
			solving.addOptions()
				CLASP_ADD(CLASP_SOLVE_OPTIONS)
				CLASP_ADD(CLASP_ENUM_OPTIONS)
				;
			OptionGroup asp("Clasp.ASP Options");
			asp.addOptions()
				CLASP_ADD(CLASP_ASP_OPTIONS)
				;
			root.add(solving);
			root.add(asp);
			OptionGroup search("Clasp.Search Options", ProgramOptions::desc_level_e1);
			search.addOptions()
				CLASP_ADD(CLASP_SOLVER_SEARCH_OPTIONS)
				CLASP_ADD(CLASP_SEARCH_OTHER_OPTIONS)
				;
			OptionGroup lookback("Clasp.Lookback Options", ProgramOptions::desc_level_e1);
			lookback.addOptions()
				CLASP_ADD(CLASP_SOLVER_LOOKBACK_OPTIONS)
				CLASP_ADD(CLASP_SEARCH_RESTART_OPTIONS)
				CLASP_ADD(CLASP_SEARCH_REDUCE_OPTIONS)
				;
			root.add(search);
			root.add(lookback);
#undef X
#undef _0
#undef CLASP_ADD_OPTION
#undef CLASP_ADD
		}
		void ClaspCliConfig::addOptions(ProgramOptions::OptionContext& root) {
			init(root);
			if (optIndex_.get() == 0) { optIndex_ = &root; optIndex_.release(); }
		}

		bool ClaspCliConfig::get(Option o, ContextParams*& ctx, SolverParams*& solver, SolveParams*& solve) {
			UserConfig* active = this->active();
			uint32      sId = cliTemp >> 2;
			if (o < option_category_search) { solver = &active->addSolver(sId); return true; }
			if (o < option_category_context) { solve = &active->addSearch(sId); return true; }
			if (o < option_category_generator) { ctx = active; return (cliTemp & mode_port) == 0; }
			return (cliTemp & 3u) == 0;
		}

#define CLASP_CASE_OPTION(n, k, a, d, x)  case opt_##k: x ;
#define CLASP_CASE(OPTION_TABLE, SELF) OPTION_TABLE(CLASP_CASE_OPTION, SELF)
		bool ClaspCliConfig::set(Clasp::Cli::Option o, const char* value) {
			using bk_lib::xconvert;
			unsigned temp;
			SolverOpts*    solver = 0;
			SearchOpts*    search = 0;
			ContextParams* ctxOpts = 0;
			if (get(o, ctxOpts, solver, search)) {
				switch (o) {
				default: break;
					CLASP_CASE(CLASP_CONTEXT_OPTIONS, (*ctxOpts))
						CLASP_CASE(CLASP_ASP_OPTIONS, asp)
						CLASP_CASE(CLASP_ENUM_OPTIONS, enumerate)
						CLASP_CASE(CLASP_SOLVE_OPTIONS, solve)
						CLASP_CASE(CLASP_SOLVER_SEARCH_OPTIONS, (*solver))
						CLASP_CASE(CLASP_SOLVER_LOOKBACK_OPTIONS, (*solver))
						CLASP_CASE(CLASP_SEARCH_RESTART_OPTIONS, search->restart)
						CLASP_CASE(CLASP_SEARCH_REDUCE_OPTIONS, search->reduce)
						CLASP_CASE(CLASP_SEARCH_OTHER_OPTIONS, (*search))
				}
			}
			error(o);
			return false;
		}
		bool ClaspCliConfig::set(ConfigOption o, const char* value) {
			if (o != opt_tester || isGenerator()) {
				uint8 temp;
				switch (o) {
				default: break;
					CLASP_CASE(CONFIG_OPTIONS, (*this))
				}
			}
			error(o);
			return false;
		}
#undef CLASP_CASE_OPTION
#undef CLASP_CASE

		void ClaspCliConfig::error(int opt) const {
#define CLASP_CASE_OPTION(n, k, a, d, x)  case opt_##k: optName = n; break;
#define CLASP_CASE(OPTION_TABLE) OPTION_TABLE(CLASP_CASE_OPTION,)
			const char* optName = "???";
			switch (opt) {
			default: break;
				CLASP_CASE(CONFIG_OPTIONS)
					CLASP_CASE(CLASP_CONTEXT_OPTIONS)
					CLASP_CASE(CLASP_ASP_OPTIONS)
					CLASP_CASE(CLASP_ENUM_OPTIONS)
					CLASP_CASE(CLASP_SOLVE_OPTIONS)
					CLASP_CASE(CLASP_SOLVER_SEARCH_OPTIONS)
					CLASP_CASE(CLASP_SOLVER_LOOKBACK_OPTIONS)
					CLASP_CASE(CLASP_SEARCH_RESTART_OPTIONS)
					CLASP_CASE(CLASP_SEARCH_REDUCE_OPTIONS)
					CLASP_CASE(CLASP_SEARCH_OTHER_OPTIONS)
			}
#undef CLASP_CASE_OPTION
#undef CLASP_CASE
			const char* end = optName;
			while (*end && *end != ',' && *end != '!') { ++end; }
			throw ProgramOptions::UnknownOption(isGenerator() ? "<clasp>" : "<tester>", std::string(optName, end - optName));
		}

		bool ClaspCliConfig::setTester(Option o, const char* value, uint32 solverId) {
			addTesterConfig();
			return ScopedSet(*this, mode_tester, solverId)->set(o, value);
		}
		bool ClaspCliConfig::setSolver(uint32 id, Option o, const char* value) {
			return ScopedSet(*this, mode_port, id)->set(o, value);
		}
		bool ClaspCliConfig::setPortfolio(PortfolioType gen, PortfolioType test) {
			this->cliPortKey = (uint8)gen;
			if (test != portfolio_auto) { addTesterConfig()->cliPortKey = (uint8)test; }
			return true;
		}
		bool ClaspCliConfig::setDefaults(UserConfig* active, uint32 sId, DefaultConfig c, const ProgramOptions::ParsedOptions& vm) {
			if (optIndex_.get() == 0) { optIndex_ = new ProgramOptions::OptionContext(); init(*optIndex_); }
			ScopedSet temp(*this, active == this ? 0 : mode_tester, sId);
			ProgramOptions::ParsedValues defs(*optIndex_);
			ProgramOptions::ParsedOptions exclude(vm);
			if (active->addSolver(sId).search == SolverParams::no_learning) {
				defs.add("heuristic", "unit");
				defs.add("lookahead", "atom");
				defs.add("deletion", "no");
				defs.add("restarts", "no");
				exclude.assign(defs);
				defs.clear();
			}
			else if (c == config_default) { c = config_frumpy; }
			defs = ProgramOptions::parseCommandString(prepareCmd(getDefaults(c).opts, active, sId), *optIndex_, true);
			int maxOption = active == this ? option_category_end : option_category_generator;
			for (ProgramOptions::ParsedValues::iterator it = defs.begin(), end = defs.end(); it != end; ++it) {
				if (exclude.count(it->first->name())) { continue; }
				ProgOption* v = static_cast<ProgOption*>(it->first->value());
				if (v->option() >= 0 && v->option() < maxOption) {
					const char* val = !it->second.empty() ? it->second.c_str() : v->implicit();
					if (!set(static_cast<Option>(v->option()), val)) { return false; }
				}
			}
			return true;
		}
		bool ClaspCliConfig::setDefaults(UserConfig* active, const ProgramOptions::ParsedOptions& x) {
			if ((active->cliDefCfg & opt_applied) == 0 && !setDefaults(active, 0, static_cast<DefaultConfig>(active->cliDefCfg), x)) {
				return false;
			}
			active->cliDefCfg |= opt_applied;
			return true;
		}
		bool ClaspCliConfig::set(const std::string& cmd, const ProgramOptions::ParsedOptions& exclude, const std::string& ctx) {
			if (optIndex_.get() == 0) { optIndex_ = new ProgramOptions::OptionContext(); init(*optIndex_); }
			using namespace ProgramOptions;
			typedef OptionContext::option_iterator opt_iter;
			Range<uint32> valid(0, optIndex_->size());
			if (!optIndex_.is_owner()) {
				valid.lo = optIndex_->find("configuration") - optIndex_->begin();
				valid.hi = valid.lo + option_category_end - 1;
			}
			std::string arg, value;
			uint64 seen[2] = { 0,0 };
			for (std::string::const_iterator it = cmd.begin(), end = cmd.end(); nextArg(it, end, arg); value.clear()) {
				std::string::size_type keyPos = std::min(arg.find_first_not_of("-"), arg.size());
				std::string::size_type valPos = arg.find("=", 2);
				if (valPos != std::string::npos) { value.assign(arg, valPos + 1, std::string::npos); arg.erase(valPos); }
				opt_iter  opt = optIndex_->tryFind(arg.c_str() + keyPos, keyPos != 1 ? OptionContext::find_name_or_prefix : OptionContext::find_alias);
				uint32     id = opt - optIndex_->begin();
				if (id < valid.lo || id >= valid.hi) { throw UnknownOption(ctx, arg); }
				ProgOption* v = static_cast<ProgOption*>((*opt)->value());
				uint64&    xs = seen[(id = static_cast<uint32>(v->option() + 4)) / 64];
				uint64      m = static_cast<uint64>(1u) << (id & 63);
				if (value.empty() && !v->isImplicit()) { nextArg(it, end, value); }
				if (exclude.count((*opt)->name())) { continue; }
				if ((xs & m) != 0 && !v->isComposing()) { throw ValueError(ctx, ValueError::multiple_occurences, (*opt)->name(), ""); }
				if (!v->parse((*opt)->name(), value, Value::value_unassigned)) { throw ValueError(ctx, ValueError::invalid_value, (*opt)->name(), value); }
				xs |= m;
			}
			return true;
		}

		bool ClaspCliConfig::initTester() {
			if (!testerCmd_.empty() || testerConfig()) {
				addTesterConfig();
				ProgramOptions::ParsedOptions ex;
				if (!testerCmd_.empty() && !ScopedSet(*this, mode_tester)->set(testerCmd_, ex, "<tester>")) {
					return false;
				}
				setDefaults(testerConfig(), ex);
				testerCmd_ = "";
				return populateSolvers(testerConfig(), ex);
			}
			return true;
		}

		bool ClaspCliConfig::populateSolvers(UserConfig* active, const ProgramOptions::ParsedOptions& exclude) {
			SolverParams defSolver = active->solver(0);
			SolveParams  defSearch = active->search(0);
			validate(defSolver, defSearch);
			uint32 pt = active->cliPortKey;
			if (pt == portfolio_auto && solve.numSolver() > 1 && solve.defaultPortfolio()) {
				pt = portfolio_def;
			}
			active->seed = pt == portfolio_seed;
			if (pt < (uint32)portfolio_def) { return true; }
			const char* portfolio = getPortfolio(pt);
			if (optIndex_.get() == 0) { optIndex_ = new ProgramOptions::OptionContext(); init(*optIndex_); }
			uint8  mode = mode_port | (active == testerConfig() ? mode_tester : 0);
			uint32 portSize = 0;
			std::string ctx = "Portfolio.";
			for (uint32 i = 0; i != solve.numSolver() && *portfolio; ++i) {
				SolverParams& solver = (active->addSolver(i) = defSolver);
				if (const char* cmd = std::strstr(portfolio, "--")) {
					SolveParams& search = (active->addSearch(i) = defSearch);
					ctx.append(std::strchr(portfolio, '['), std::strchr(portfolio, ']') + 1);
					ScopedSet temp(*this, mode, i);
					if (!temp->set(prepareCmd(cmd, active, i), exclude, ctx)) { return false; }
					try { validate(solver, search); }
					catch (const std::logic_error& e) { ctx.append(": ").append(e.what()); throw std::logic_error(ctx); }
					ctx.erase(10);
				}
				++portSize;
				portfolio += (std::strlen(portfolio) + 1);
			}
			RNG r(14182940 + portSize);
			for (uint32 i = portSize; i != solve.numSolver(); ++i, r.rand()) {
				active->addSolver(i) = active->solver(i % portSize);
				active->addSolver(i).seed = r.seed();
			}
			active->cliPortKey = (uint8)releasePortfolio(pt);
			return true;
		}

		bool ClaspCliConfig::finalize(const ProgramOptions::ParsedOptions& x) {
			if (!setDefaults(this, x)) { return false; }
			if (!populateSolvers(this, x)) { return false; }
			if (!initTester()) { return false; }
			return true;
		}
		bool ClaspCliConfig::finalize() {
			ProgramOptions::ParsedOptions ex;
			if (!populateSolvers(this, ex)) { return false; }
			return (!testerConfig() && solve.numSolver() == 1) || populateSolvers(addTesterConfig(), ex);
		}
		std::string ClaspCliConfig::prepareCmd(const char* cmd, UserConfig* active, uint32 sId) const {
			std::string ret(cmd);
			if (active->search(sId).reduce.fReduce() == 0.0f) {
				std::size_t pos;
				if ((pos = ret.find("--del-grow")) != std::string::npos) { ret.erase(pos, ret.find("--", pos + 2) - pos); }
				if ((pos = ret.find("--del-cfl")) != std::string::npos) { ret.erase(pos, ret.find("--", pos + 2) - pos); }
				if ((pos = ret.find("--del-max")) != std::string::npos) { ret.erase(pos, ret.find("--", pos + 2) - pos); }
			}
			return ret;
		}

		void ClaspCliConfig::setDefaults(ProblemType t) {
			if (cliSatPre == 0) {
				SatPreParams opts;
				if (t != Problem_t::ASP) {
					opts.type = SatPreParams::sat_pre_ve_bce;
					opts.limIters = 20;
					opts.limOcc = 25;
					opts.limTime = 120;
				}
				satPre = opts;
			}
		}
		const char* ClaspCliConfig::getDefaults(ProblemType t) {
			if (t == Problem_t::ASP) { return "--eq=5"; }
			else { return "--sat-prepro=20,25,120"; }
		}

		void validate(const SolverParams& solver, const SolveParams& search) {
			if (solver.search == SolverParams::no_learning) {
				if (Heuristic_t::isLookback(solver.heuId)) { throw std::logic_error("Heuristic requires lookback strategy!"); }
				SolveParams def;
				if (std::memcmp(&search.restart, &def.restart, sizeof(RestartParams)) != 0 && !search.restart.sched.disabled()) {
					throw std::logic_error("'no-lookback': restart options are disabled!");
				}
				if (std::memcmp(&search.reduce, &def.reduce, sizeof(ReduceParams)) != 0) {
					if (!search.reduce.cflSched.disabled() || !search.reduce.growSched.disabled() || search.reduce.fReduce() != 0) {
						throw std::logic_error("'no-lookback': deletion options are disabled!");
					}
				}
			}
			const ReduceParams& reduce = search.reduce;
			bool  hasSched = !reduce.cflSched.disabled() || !reduce.growSched.disabled() || reduce.maxRange != UINT32_MAX;
			if (reduce.fReduce() == 0.0f && hasSched && !reduce.growSched.defaulted()) {
				throw std::logic_error("'no-deletion': deletion strategies are disabled!");
			}
			if (reduce.fReduce() != 0.0f && !hasSched && !reduce.growSched.defaulted()) {
				throw std::logic_error("'deletion': deletion strategy required!");
			}
		}
		/////////////////////////////////////////////////////////////////////////////////////////
		// Portfolios & Default Configs
		/////////////////////////////////////////////////////////////////////////////////////////
#define DEF_SOLVE    "--heuristic=Berkmin --restarts=x,100,1.5 --deletion=basic,75 --del-init=3.0,200,40000 --del-max=400000 --contraction=250 --loops=common --save-p=180"
#define FRUMPY_SOLVE DEF_SOLVE " --del-grow=1.1 --strengthen=local --sign-def=4"
#define JUMPY_SOLVE  "--heuristic=Vsids --restarts=L,100 --deletion=basic,75,2 --del-init=3.0,1000,20000 --del-grow=1.1,25,x,100,1.5 --del-cfl=x,10000,1.1 --del-glue=2 --update-lbd=3 --strengthen=recursive --otfs=2 --save-p=70"
#define HANDY_SOLVE  "--heuristic=Vsids --restarts=D,100,0.7 --deletion=sort,50,2 --del-max=200000 --del-init=20.0,1000,14000 --del-cfl=+,4000,600 --del-glue=2 --update-lbd --strengthen=recursive --otfs=2 --save-p=20 --contraction=600 --loops=distinct --counter-restarts=7 --counter-bump=1023 --reverse-arcs=2"
#define CRAFTY_SOLVE "--heuristic=Vsids --restarts=x,128,1.5 --deletion=basic,75,0 --del-init=10.0,1000,9000 --del-grow=1.1,20.0 --del-cfl=+,10000,1000 --del-glue=2 --otfs=2 --reverse-arcs=1 --counter-restarts=3 --contraction=250"
#define TRENDY_SOLVE "--heuristic=Vsids --restarts=D,100,0.7 --deletion=basic,50,0 --del-init=3.0,500,19500 --del-grow=1.1,20.0,x,100,1.5 --del-cfl=+,10000,2000 --del-glue=2 --strengthen=recursive --update-lbd --otfs=2 --save-p=75 --counter-restarts=3 --counter-bump=1023 --reverse-arcs=2  --contraction=250 --loops=common"
#define JUMPY_GLOBAL "--sat-p=20,25,240,-1,1 --trans-ext=dynamic"
#define HANDY_GLOBAL "--sat-p=10,25,240,-1,1 --trans-ext=dynamic --backprop"
		ClaspCliConfig::NamedConfig ClaspCliConfig::getDefaults(DefaultConfig c) {
			static NamedConfig configs[config_max_value + 1] = {
				{"default", ""},
				{"frumpy", FRUMPY_SOLVE},
				{"jumpy", JUMPY_GLOBAL " " JUMPY_SOLVE},
				{"handy", HANDY_GLOBAL " " HANDY_SOLVE},
				{"crafty", HANDY_GLOBAL " " CRAFTY_SOLVE " --save-p=180"},
				{"trendy", JUMPY_GLOBAL " " TRENDY_SOLVE}
			};
			return configs[c];
		}
		static const char* const default_port_g = {
			/*     0 */"[CRAFTY]: " CRAFTY_SOLVE " --opt-heu=1"
		#if WITH_THREADS
			"\0"/* 1 */"[TRENDY]: " TRENDY_SOLVE " --opt-heu=1"
			"\0"/* 2 */"[FRUMPY]: " FRUMPY_SOLVE
			"\0"/* 3 */"[JUMPY]:  " JUMPY_SOLVE " --opt-heu=3"
			"\0"/* 4 */"[STRONG]: " DEF_SOLVE " --berk-max=512 --del-grow=1.1,25 --otfs=2 --reverse-arcs=2 --strengthen=recursive --init-w=2 --lookahead=atom,10"
			"\0"/* 5 */"[HANDY]:  " HANDY_SOLVE
			"\0"/* 6 */"[S2]: --heuristic=Vsids --reverse-arcs=1 --otfs=1 --local-restarts --save-progress=0 --contraction=250 --counter-restart=7 --counter-bump=200 --restarts=x,100,1.5 --del-init=3.0,800,-1 --deletion=basic,60,0 --strengthen=local --del-grow=1.0,1.0 --del-glue=4 --del-cfl=+,4000,300,100"
			"\0"/* 7 */"[S4]: --heuristic=Vsids --restarts=L,256 --counter-restart=3 --strengthen=recursive --update-lbd --del-glue=2 --otfs=2 --deletion=inp_sort,75,2 --del-init=20.0,1000,19000"
			"\0"/* 8 */"[SLOW]:  --heuristic=Berkmin --berk-max=512 --restarts=F,16000 --lookahead=atom,50"
			"\0"/* 9 */"[VMTF]:  --heu=VMTF --str=no --contr=0 --restarts=x,100,1.3 --del-init=3.0,800,9200"
			"\0"/* 10 */"[SIMPLE]:  --heu=VSIDS  --strengthen=recursive --restarts=x,100,1.5,15 --contraction=0"
			"\0"/* 11*/"[LUBY-SP]: --heu=VSIDS --restarts=L,128 --save-p --otfs=1 --init-w=2 --contr=0 --opt-heu=3"
			"\0"/* 12 */"[LOCAL-R]: --berk-max=512 --restarts=x,100,1.5,6 --local-restarts --init-w=2 --contr=0"
		#endif
			"\0"
		};
		ClaspCliConfig::PortVec ClaspCliConfig::portfolios_g;
		static void addPortConfig(std::string& out, const std::string& line) {
			std::string::size_type nBeg = line.find("[");
			std::string::size_type nEnd = line.find("]:");
			if (nBeg != std::string::npos && nEnd != std::string::npos && nBeg < nEnd) {
				out.append(line, nBeg, line.size() - nBeg);
				out.append(1, '\0');
				return;
			}
			throw std::logic_error(std::string("Invalid portfolio config '").append(line).append("'!"));
		}
		unsigned ClaspCliConfig::loadPortfolio(const char* name) {
			portfolios_g.reserve(2);
			unsigned key = (unsigned)portfolios_g.size() + portfolio_usr;
			std::ifstream file(name);
			if (!file) { throw std::logic_error(std::string("Could not open portfolio file '").append(name).append("'")); }
			portfolios_g.push_back("");
			std::string& port = portfolios_g.back();
			port.reserve(128);
			for (std::string line; std::getline(file, line); ) {
				if (line.empty() || line[0] == '#') { continue; }
				try { addPortConfig(port, line); }
				catch (...) { portfolios_g.pop_back(); throw; }
			}
			port.append(1, '\0');
			return key;
		}
		unsigned ClaspCliConfig::addPortfolio(const char** configs, unsigned size) {
			portfolios_g.reserve(2);
			unsigned key = (unsigned)portfolios_g.size() + portfolio_usr;
			portfolios_g.push_back("");
			std::string& port = portfolios_g.back();
			port.reserve(128);
			for (unsigned i = 0; i != size; ++i) {
				try { addPortConfig(port, configs[i]); }
				catch (...) { portfolios_g.pop_back(); throw; }
			}
			port.append(1, '\0');
			return key;
		}
		const char* ClaspCliConfig::getPortfolio(unsigned key) {
			switch (key) {
			case portfolio_auto:
			case portfolio_no:
			case portfolio_seed: return 0;
			case portfolio_def: return default_port_g;
			default: key -= (unsigned)portfolio_usr; return portfolios_g.at(key).c_str();
			}
		}
		unsigned ClaspCliConfig::releasePortfolio(unsigned key) {
			if (key >= (unsigned)portfolio_usr) {
				key -= (unsigned)portfolio_usr;
				portfolios_g.at(key).clear();
				while (!portfolios_g.empty() && portfolios_g.back().empty()) { portfolios_g.pop_back(); }
			}
			return portfolio_auto;
		}
	}
}
