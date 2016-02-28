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
#ifndef CLASP_CLI_CLASP_OPTIONS_H_INCLUDED
#define CLASP_CLI_CLASP_OPTIONS_H_INCLUDED

#ifdef _MSC_VER
#pragma warning (disable : 4200) // nonstandard extension used : zero-sized array
#pragma once
#endif

#include "../../clasp/cli/clifwd.h"
#include "../../clasp/clasp_facade.h"
#include <string>
namespace Clasp {
	namespace Cli {

#define CLASP_DECLARE_OPTION_KEY(n,k,...) opt_##k,
#define CLASP_DECLARE_OPTION_KEYS(OPTION_TABLE) \
	OPTION_TABLE(CLASP_DECLARE_OPTION_KEY,)

		enum Option {
			option_category_solver,
			CLASP_DECLARE_OPTION_KEYS(CLASP_SOLVER_SEARCH_OPTIONS)
			CLASP_DECLARE_OPTION_KEYS(CLASP_SOLVER_LOOKBACK_OPTIONS)
			option_category_search,
			CLASP_DECLARE_OPTION_KEYS(CLASP_SEARCH_RESTART_OPTIONS)
			CLASP_DECLARE_OPTION_KEYS(CLASP_SEARCH_REDUCE_OPTIONS)
			CLASP_DECLARE_OPTION_KEYS(CLASP_SEARCH_OTHER_OPTIONS)
			option_category_context,
			CLASP_DECLARE_OPTION_KEYS(CLASP_CONTEXT_OPTIONS)
			option_category_generator,
			CLASP_DECLARE_OPTION_KEYS(CLASP_ASP_OPTIONS)
			CLASP_DECLARE_OPTION_KEYS(CLASP_ENUM_OPTIONS)
			CLASP_DECLARE_OPTION_KEYS(CLASP_SOLVE_OPTIONS)
			option_category_end
		};

#undef CLASP_DECLARE_OPTION_KEY
#undef CLASP_DECLARE_OPTION_KEYS

		class ClaspCliConfig : public ClaspConfig {
		public:
			//! Default configurations.
			enum DefaultConfig {
				config_default = 0, config_frumpy = 1, config_jumpy = 2, config_handy = 3,
				config_crafty = 4, config_trendy = 5, config_max_value = 5
			};
			//! Portfolio to use when configuring solvers.
			enum PortfolioType {
				portfolio_auto = 0u, portfolio_no = 1u, portfolio_seed = 2u, portfolio_def = 3u, portfolio_usr = 4u
			};
			static unsigned    loadPortfolio(const char* fileName);
			static unsigned    addPortfolio(const char** configs, unsigned size);
			static const char* getPortfolio(unsigned key);
			static unsigned    releasePortfolio(unsigned key);

			ClaspCliConfig();
			/*!
			 * \name Raw interface
			 */
			 //@{
			 //! Initializes the i'th solver with the given default configuration.
			void init(uint32 solverId = 0, DefaultConfig generator = config_frumpy, DefaultConfig tester = config_default);
			//! Sets the given option to the given value. 
			bool set(Option o, const char* value);
			//! Sets the given option in the tester solver.
			bool setTester(Option o, const char* value, uint32 solverId = 0);
			//! Sets the given option in the i'th solver. 
			bool setSolver(uint32 i, Option o, const char* value);
			//! Sets portfolios to use for initializing solvers.
			bool setPortfolio(PortfolioType generator, PortfolioType tester = portfolio_auto);
			//! Validates and finalizes this configuration.
			bool finalize();
			//@}

			/*!
			 * \name App interface
			 */
			 //@{
			 //! Adds all available options to root.
			 /*!
			  * Once options are added, an option source (e.g. the command-line)
			  * can be used to populate this object.
			  */
			void addOptions(ProgramOptions::OptionContext& root);
			//! Applies the options in parsed and calls finalize().
			bool finalize(const ProgramOptions::ParsedOptions& parsed);
			//@}
			struct NamedConfig {
				const char* name;
				const char* opts;
			};
			virtual void       setDefaults(ProblemType t);
			static const char* getDefaults(ProblemType f);
			static NamedConfig getDefaults(DefaultConfig c);
		private:
			enum ConfigOption { opt_configuration = -1, opt_portfolio = -2, opt_tester = -3 };
			static const uint8 mode_port = 1u;
			static const uint8 mode_tester = 2u;
			static const uint8 opt_applied = 0x80u;
			typedef SingleOwnerPtr<ProgramOptions::OptionContext> OptCtxPtr;
			typedef PodVector<std::string>::type PortVec;
			struct ScopedSet {
				ScopedSet(ClaspCliConfig& s, uint8 mode, uint32 sId = 0);
				~ScopedSet();
				ClaspCliConfig* operator->()const { return self; }
				ClaspCliConfig* self;
			};
			class ProgOption;
			bool get(Option o, CtxOpts*& ctx, SolverParams*& solver, SolveParams*& solve);
			void init(ProgramOptions::OptionContext& root);
			bool setDefaults(UserConfig* active, uint32 sId, DefaultConfig c, const ProgramOptions::ParsedOptions& exclude);
			bool setDefaults(UserConfig* active, const ProgramOptions::ParsedOptions& exclude);
			bool set(ConfigOption o, const char* value);
			bool set(const std::string& cmd, const ProgramOptions::ParsedOptions& exclude, const std::string& ctx);
			std::string prepareCmd(const char* cmd, UserConfig* active, uint32 id) const;
			void        error(int opt)const;
			bool        isGenerator() const { return (cliTemp & mode_tester) == 0; }
			UserConfig* active() { return isGenerator() ? this : testerConfig(); }
			const UserConfig* active()const { return isGenerator() ? this : testerConfig(); }
			bool        initTester();
			bool        populateSolvers(UserConfig* active, const ProgramOptions::ParsedOptions& exclude);
			ProgOption* createOption(int o);
			OptCtxPtr   optIndex_;
			std::string testerCmd_;
			static PortVec portfolios_g;
		};
		void validate(const SolverParams& solver, const SolveParams& search);

	}
}
#endif
