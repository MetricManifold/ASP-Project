// {{{ GPL License 

// This file is part of gringo - a grounder for logic programs.
// Copyright (C) 2013  Benjamin Kaufmann
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

#include "clingo_app.hh"
#include "../../../libclasp/clasp/parser.h"
#include <climits>
#include <unistd.h>

using namespace Clasp;
using namespace Clasp::Cli;

// {{{ declaration of ClingoApp

Gringo::Statistics::Quantity ClingoStatistics::getStat(char const* key) const {
	if (!clasp) { return std::numeric_limits<double>::quiet_NaN(); }
	auto ret = clasp->getStat(key);
	switch (ret.error()) {
	case Clasp::ExpectedQuantity::error_ambiguous_quantity: { return Gringo::Statistics::error_ambiguous_quantity; }
	case Clasp::ExpectedQuantity::error_not_available: { return Gringo::Statistics::error_not_available; }
	case Clasp::ExpectedQuantity::error_unknown_quantity: { return Gringo::Statistics::error_unknown_quantity; }
	case Clasp::ExpectedQuantity::error_none: { return (double)ret; }
	}
	return std::numeric_limits<double>::quiet_NaN();
}
char const *ClingoStatistics::getKeys(char const* key) const {
	if (!clasp) { return ""; }
	return clasp->getKeys(key);
}

ClingoApp::ClingoApp() {
}

void ClingoApp::initOptions(ProgramOptions::OptionContext& root) {
	using namespace ProgramOptions;
	BaseType::initOptions(root);
	grOpts_.defines.clear();
	grOpts_.verbose = false;
	OptionGroup gringo("Gringo Options");
	gringo.addOptions()
		("text", flag(grOpts_.text = false), "Print plain text format")
		("const,c", storeTo(grOpts_.defines)->composing()->arg("<id>=<term>"), "Replace term occurences of <id> with <term>")
		("lparse-rewrite", flag(grOpts_.lpRewrite = false), "Use together with --text to inspect lparse rewriting")
		("lparse-debug", storeTo(grOpts_.lparseDebug = Gringo::Output::LparseDebug::NONE, values<Gringo::Output::LparseDebug>()
			("none", Gringo::Output::LparseDebug::NONE)
			("plain", Gringo::Output::LparseDebug::PLAIN)
			("lparse", Gringo::Output::LparseDebug::LPARSE)
			("all", Gringo::Output::LparseDebug::ALL)), "Debug information during lparse rewriting:\n"
			"      none  : no additional info\n"
			"      plain : print rules as in plain output (prefix %%)\n"
			"      lparse: print rules as in lparse output (prefix %%%%)\n"
			"      all   : combines plain and lparse\n")
		("Wno-atom-undefined", flag(grOpts_.wNoAtomUndef = false), "a :- b.")
		("Wno-define-cyclic", flag(grOpts_.wNoCycle = false), "#const a=b. #const b=a.")
		("Wno-define-redfinition", flag(grOpts_.wNoRedef = false), "#const a=1. #const a=2.")
		("Wno-file-included", flag(grOpts_.wNoFileIncluded = false), "#include \"a.lp\". #include \"a.lp\".")
		("Wno-nonmonotone-aggregate", flag(grOpts_.wNoNonMonotone = false), "a :- #sum { 1:a; -1:a } >= 0.")
		("Wno-term-undefined", flag(grOpts_.wNoTermUndef = false), "p(1/0).")
		;
	root.add(gringo);

	OptionGroup basic("Basic Options");
	basic.addOptions()
		("mode", storeTo(mode_ = mode_clingo, values<Mode>()("clingo", mode_clingo)("clasp", mode_clasp)("gringo", mode_gringo)), "Run in {clingo|clasp|gringo} mode\n")
		;
	root.add(basic);
}

void ClingoApp::validateOptions(const ProgramOptions::OptionContext& root, const ProgramOptions::ParsedOptions& parsed) {
	BaseType::validateOptions(root, parsed);
	if (grOpts_.text) {
		if (mode_ == mode_clasp) { error("'--text' and '--mode=clasp' are mutually exclusive!"); exit(E_NO_RUN); }
		mode_ = mode_gringo;
	}
}

ProblemType ClingoApp::getProblemType() {
	if (mode_ != mode_clasp) return Problem_t::ASP;
	InputFormat input = Input_t::detectFormat(getStream());
	return Problem_t::format2Type(input);
}
Output* ClingoApp::createOutput(ProblemType f) {
	if (mode_ == mode_gringo) return 0;
	return BaseType::createOutput(f);
}


void ClingoApp::printHelp(const ProgramOptions::OptionContext& root) {
	BaseType::printHelp(root);
	printf("\nclingo is part of Potassco: %s\n", "http://potassco.sourceforge.net/#clingo");
	printf("Get help/report bugs via : http://sourceforge.net/projects/potassco/support\n");
	fflush(stdout);
}

void ClingoApp::printVersion() {
	ProgramOptions::Application::printVersion();
	printf("\n");
	printf("libgringo version " GRINGO_VERSION "\n");
	printf("Copyright (C) Roland Kaminski\n");
	printf("License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n");
	printf("Gringo is free software: you are free to change and redistribute it.\n");
	printf("There is NO WARRANTY, to the extent permitted by law.\n");
	printf("\n");
	BaseType::printLibClaspVersion();
}
namespace {

	struct ClingoModel : Gringo::Model {
		ClingoModel(Clasp::Asp::LogicProgram const &lp, Gringo::Output::OutputBase const &out, Clasp::Model const &model)
			: lp(lp)
			, out(out)
			, model(model) { }
		virtual bool contains(Gringo::Value atom) const {
			auto atm = out.find(atom);
			return atm && model.isTrue(lp.getLiteral(atm->uid()));
		}
		virtual Gringo::ValVec atoms(int atomset) const {
			return out.atoms(atomset, [this](unsigned uid) { return model.isTrue(lp.getLiteral(uid)); });
		}
		virtual ~ClingoModel() { }
		Clasp::Asp::LogicProgram const   &lp;
		Gringo::Output::OutputBase const &out;
		Clasp::Model const               &model;
	};

} // namespace
bool ClingoApp::onModel(Clasp::Solver const& s, Clasp::Model const& m) {
	bool ret = !modelHandler || modelHandler(ClingoModel(static_cast<Clasp::Asp::LogicProgram&>(*clasp_->program()), *grd->out, m));
	return BaseType::onModel(s, m) && ret;
}
Gringo::Value ClingoApp::getConst(std::string const &name) {
	auto ret = grd->defs.defs().find(name);
	return ret != grd->defs.defs().end() && std::get<2>(ret->second)->getInvertibility() == Gringo::Term::CONSTANT
		? std::get<2>(ret->second)->eval()
		: Gringo::Value();
}

void ClingoApp::ground(std::string const &name, Gringo::FWValVec args) {
	params.add(name, args);
}
void ClingoApp::add(std::string const &name, Gringo::FWStringVec const &params, std::string const &part) {
	Gringo::Location loc("<block>", 1, 1, "<block>", 1, 1);
	Gringo::Input::IdVec idVec;
	for (auto &x : params) { idVec.emplace_back(loc, x); }
	parts.emplace_back(name, std::move(idVec), part);
}
void ClingoApp::setConf(std::string const &part, bool replace) {
	configStr = part;
	configUpdate = replace ? ConfigUpdate::REPLACE : ConfigUpdate::UPDATE;
}
bool ClingoApp::prepare(ModelHandler h) {
	modelHandler = h;
	if (!clasp_->program()) { return false; }
	if (configUpdate != ConfigUpdate::KEEP) {
		ClaspAppBase::updateConfig(configStr, configUpdate == ConfigUpdate::REPLACE);
	}
	Asp::LogicProgram& prg = static_cast<Asp::LogicProgram&>(clasp_->update(configUpdate != ConfigUpdate::KEEP));
	configUpdate = ConfigUpdate::KEEP;
#ifdef DEBUG_OUTPUT
	std::cerr << "clasp.update();\n";
#endif
	if (!prg.ok()) { return false; }
	grd->ground(params, parts);
	if (!grd->lpOut || grd->lpOut->disposeMinimize()) { prg.disposeMinimizeConstraint(); }
	for (auto &ext : freeze) {
		Gringo::AtomState const* atm;
		if (grd->out && (atm = grd->out->find(ext.first)) && atm->hasUid()) {
#ifdef DEBUG_OUTPUT
			std::cerr << "pb.freeze(" << atm->uid() << "," << (ext.second ? "Clasp::value_true" : "Clasp::value_false") << ");\n";
#endif
			prg.freeze(atm->uid(), ext.second ? Clasp::value_true : Clasp::value_false);
		}
	}
	freeze.clear();
	for (auto &ext : unfreeze) {
		Gringo::AtomState const * atm;
		if (grd->out && (atm = grd->out->find(ext)) && atm->hasUid()) {
#ifdef DEBUG_OUTPUT
			std::cerr << "pb.unfreeze(" << atm->uid() << ");\n";
#endif
			prg.unfreeze(atm->uid());
		}
	}
	unfreeze.clear();
	if (grOpts_.text) { exit(EXIT_SUCCESS); }
	if (!handlePostGroundOptions(prg)) { return false; }
#ifdef DEBUG_OUTPUT
	std::cerr << "clasp.prepare(ClaspFacade::dispose_no);\n";
#endif
	return clasp_->prepare(enableEnumAssupmption_ ? ClaspFacade::enum_volatile : ClaspFacade::enum_static) && handlePreSolveOptions(*clasp_);
}
Gringo::SolveFuture *ClingoApp::asolve(ModelHandler mh, FinishHandler fh) {
#if WITH_THREADS
	prepare(mh);
#ifdef DEBUG_OUTPUT
	static bool defined = false;
	if (!defined) {
		std::cerr << "Clasp::ClaspFacade::AsyncResult f = clasp.solveAsync();\n";
		defined = true;
	}
	else {
		std::cerr << "f = clasp.solveAsync();\n";
	}
#endif
	solveFuture_.reset(fh);
	solveFuture_.reset(clasp_->solveAsync());
	return &solveFuture_;
#else
	(void)mh;
	(void)fh;
	throw std::runtime_error("asolve requires clingo to be build with thread support");
#endif
}
bool ClingoApp::blocked() {
	return clasp_->solving();
}
void ClingoApp::onEvent(Event const& ev) {
#if WITH_THREADS
	Clasp::ClaspFacade::StepReady const *r = Clasp::event_cast<Clasp::ClaspFacade::StepReady>(ev);
	if (r) {
#ifdef DEBUG_OUTPUT
		std::cerr <<
			"switch (f.get()) {\n"
			"    case ClaspFacade::Result::SAT:     { printf(\"SAT\\n\"); break; }\n"
			"    case ClaspFacade::Result::UNSAT:   { printf(\"UNSAT\\n\"); break; }\n"
			"    case ClaspFacade::Result::UNKNOWN: { printf(\"UNKNOWN\\n\"); break; }\n"
			"}\n";
#endif
		solveFuture_.ready(r->summary->result);
	}
#endif
	BaseType::onEvent(ev);
}
Gringo::SolveResult ClingoApp::solve(ModelHandler h) {
	if (!prepare(h)) { return convert(clasp_->result()); }
#ifdef DEBUG_OUTPUT
	std::cerr << "clasp.solve();\n";
#endif
	return convert(clasp_->solve());
}
std::string ClingoApp::str() {
	return "[object:IncrementalControl]";
}
void ClingoApp::assignExternal(Gringo::Value ext, bool val) {
	freeze.emplace_back(ext, val);
}
void ClingoApp::releaseExternal(Gringo::Value ext) {
	unfreeze.emplace_back(ext);
}

ClingoStatistics *ClingoApp::getStats() {
	clingoStats.clasp = clasp_.get();
	return &clingoStats;
}
void ClingoApp::enableEnumAssumption(bool enable) {
	enableEnumAssupmption_ = enable;
}

void ClingoApp::run(Clasp::ClaspFacade& clasp) {
	bool incremental = mode_ != mode_clasp;
	ProblemType     pt = getProblemType();
	ProgramBuilder* prg = &clasp.start(claspConfig_, pt, incremental);
#ifdef DEBUG_OUTPUT
	std::cerr
		<< "#include <clasp/clasp_facade.h>\n"
		<< "#include <cstdio>\n"
		<< "using namespace Clasp;\n"
		<< "int main() {\n"
		<< "ClaspFacade clasp;\n"
		<< "ClaspConfig cfg;\n"
		<< "Asp::LogicProgram& pb = static_cast<Asp::LogicProgram&>(clasp.start(cfg, Problem_t::ASP, true));\n";
#endif
	if (incremental) {
		grOpts_.verbose = verbose() == UINT_MAX;
		Asp::LogicProgram* lp = mode_ != mode_gringo ? static_cast<Asp::LogicProgram*>(prg) : 0;
		grd = Gringo::make_unique<Grounder>();
		grd->parse(claspAppOpts_.input, grOpts_, lp);
		grd->main(*this);
	}
	else if (prg->parseProgram(getStream()) && handlePostGroundOptions(*prg)) {
		if (clasp.prepare() && handlePreSolveOptions(clasp)) {
			clasp.solve();
		}
	}
}



// }}}

