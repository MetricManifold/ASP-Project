// {{{ GPL License 

// This file is part of gringo - a grounder for logic programs.
// Copyright (C) 2013  Roland Kaminski

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// }}}

#ifndef _GRINGO_OUTPUT_TEST_SOLVER_HELPER_HH
#define _GRINGO_OUTPUT_TEST_SOLVER_HELPER_HH

#include "../../gringo/logger.hh"
#include "../../gringo/ground/dependency.hh"
#include "../../gringo/input/nongroundparser.hh"
#include "../../gringo/input/program.hh"
#include "../../gringo/output/output.hh"
#include "../../gringo/scripts.hh"

#include "../../../libclasp/clasp/logic_program.h"
#include "../../../libclasp/clasp/solver.h"
#include "../../../libclasp/clasp/minimize_constraint.h"

#include "../../../libclasp/clasp/model_enumerators.h"
#include "../../../libclasp/clasp/solve_algorithms.h"

namespace Gringo {
	namespace Output {
		namespace Test {

			// {{{ definition of solve

			using Model = std::vector<std::string>;
			using Models = std::vector<Model>;

			inline Models solve(std::function<bool(OutputBase &, Scripts &, Input::Program&, Input::NonGroundParser &)> ground, std::string &&str, std::initializer_list<std::string> filter = { "" }, std::initializer_list<Clasp::wsum_t> minimize = {}) {
				// grounder: setup
				std::stringstream ss;
				PlainLparseOutputter plo(ss);
				OutputBase out({}, plo);
				Input::Program prg;
				Defines defs;
				Scripts scripts;
				Input::NongroundProgramBuilder pb(scripts, prg, out, defs);
				Input::NonGroundParser parser(pb);
				parser.pushStream("-", make_unique<std::stringstream>(std::move(str)));
				Models models;
				// grounder: parse
				parser.parse();
				// grounder: preprocess
				prg.rewrite(defs);
				prg.check();
				struct D { void operator()(Clasp::MinimizeConstraint* p) const { p->destroy(nullptr, false); } };
				std::unique_ptr<Clasp::MinimizeConstraint, D> mc;
				if (ground(out, scripts, prg, parser)) {
					// solver: setup
					Clasp::SharedContext ctx;
					Clasp::Asp::LogicProgram api;
					Clasp::SolveParams params;
					api.start(ctx);
					// solver: parse
					if (api.parseProgram(ss) && api.endProgram()) {
						// solver: create minimize constraint
						if (minimize.size()) {
							Clasp::SharedMinimizeData* nm = api.getMinimizeConstraint();
							assert(nm);
							if (nm->setMode(Clasp::MinimizeMode_t::enumerate, Clasp::SumVec(begin(minimize), end(minimize)))) { mc.reset(nm->attach(*ctx.master())); }
						}
						// solver: enumerate
						Clasp::ModelEnumerator enumerator;
						enumerator.init(ctx, 0);
						if (ctx.endInit()) {
							Clasp::BasicSolve solve(*ctx.master());
							if (enumerator.update(solve.solver())) {
								for (; enumerator.hasModel(solve.solver()) || solve.solve() == Clasp::value_true; ) {
									Clasp::Model m = enumerator.commitModel(solve.solver());
									models.emplace_back();
									const Clasp::SymbolTable& symTab = solve.solver().sharedContext()->symTab();
									for (Clasp::SymbolTable::const_iterator it = symTab.begin(); it != symTab.end(); ++it) {
										if (m.isTrue(it->second.lit) && !it->second.name.empty()) {
											std::string atom(it->second.name.c_str());
											for (auto &x : filter) {
												if (!atom.compare(0, x.size(), x)) {
													models.back().emplace_back(std::move(atom));
													break;
												}
											}
										}
									}
									std::sort(models.back().begin(), models.back().end());
									if (!enumerator.update(solve.solver())) { break; }
								}
							}
						}

					}
				}
				std::sort(models.begin(), models.end());
				return std::move(models);
			}

			inline Models solve(std::string &&str, std::initializer_list<std::string> filter = { "" }, std::initializer_list<Clasp::wsum_t> minimize = {}) {
				auto ground = [](OutputBase &out, Scripts &scripts, Input::Program &prg, Input::NonGroundParser &) -> bool {
					// grounder: ground
					if (!message_printer()->hasError()) {
						Ground::Program gPrg(prg.toGround(out.domains));
						gPrg.ground(scripts, out);
						return true;
					}
					return false;
				};
				return solve(ground, std::move(str), filter, minimize);
			}

			// }}}

		}
	}
} // namespace Test Output Gringo

#endif // _GRINGO_OUTPUT_TEST_SOLVER_HELPER_HH
