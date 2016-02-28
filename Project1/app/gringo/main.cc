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

#include "../../libgringo/gringo/input/nongroundparser.hh"
#include "../../libgringo/gringo/input/programbuilder.hh"
#include "../../libgringo/gringo/input/program.hh"
#include "../../libgringo/gringo/ground/program.hh"
#include "../../libgringo/gringo/output/output.hh"
#include "../../libgringo/gringo/logger.hh"
#include "../../libgringo/gringo/scripts.hh"
#include "../../libgringo/gringo/version.hh"
#include "../../libgringo/gringo/control.hh"
#include <iostream>
#include <stdexcept>

bool g_verbose = false;
#define LOG if (g_verbose) std::cerr

struct IncrementalControl : Gringo::Control {
	using StringVec = std::vector<std::string>;
	IncrementalControl() { }
	virtual void ground(std::string const &name, Gringo::FWValVec args) { params.add(name, args); }
	virtual void add(std::string const &name, Gringo::FWStringVec const &params, std::string const &part) {
		Gringo::Location loc("<block>", 1, 1, "<block>", 1, 1);
		Gringo::Input::IdVec idVec;
		for (auto &x : params) { idVec.emplace_back(loc, x); }
		parts.emplace_back(name, std::move(idVec), part);
	}
	virtual Gringo::Value getConst(std::string const &name) {
		auto ret = defs.defs().find(name);
		if (ret != defs.defs().end() && std::get<2>(ret->second)->getInvertibility() == Gringo::Term::CONSTANT) {
			return std::get<2>(ret->second)->eval();
		}
		return Gringo::Value();
	}
	virtual void onModel(Gringo::Model const &) { }
	virtual bool blocked() { return false; }
	virtual Gringo::SolveResult solve(ModelHandler) { return Gringo::SolveResult::UNKNOWN; }
	virtual Gringo::SolveFuture *asolve(ModelHandler, FinishHandler) { throw std::runtime_error("solving not supported in gringo"); }
	virtual Gringo::Statistics *getStats() { throw std::runtime_error("statistics not supported in gringo (yet)"); }
	virtual void assignExternal(Gringo::Value, bool) { }
	virtual void releaseExternal(Gringo::Value) { }
	virtual void setConf(std::string const &, bool) { }
	virtual void enableEnumAssumption(bool) { }
	virtual ~IncrementalControl() { }
	Gringo::Defines            defs;
	Gringo::Ground::Parameters params;
	Gringo::Input::ProgramVec  parts;
};

Gringo::Ground::Program prepare(IncrementalControl &inc, Gringo::Scripts &scripts, Gringo::Output::OutputBase &out, std::vector<char const *> files, std::vector<char const *> defines) {
	using namespace Gringo;
	Input::Program prg;
	Input::NongroundProgramBuilder pb(scripts, prg, out, inc.defs);
	Input::NonGroundParser parser(pb);
	for (auto &x : defines) {
		LOG << "define: " << x << std::endl;
		parser.parseDefine(x);
	}
	// The for loop looks through each file in the stack and pushes it to the parser
	for (auto &x : files) {
		LOG << "file: " << x << std::endl;
		parser.pushFile(x);		// each file is pushed to the parser, variable filenames_
	}
	if (files.empty()) {
		LOG << "reading from stdin" << std::endl;
		parser.pushFile("-");
	}
	parser.parse();
	if (scripts.callable("main")) {
		scripts.main(inc);
		if (!inc.parts.empty()) {
			parser.pushBlocks(std::move(inc.parts));
			parser.parse();
		}
	}
	LOG << "************** parsed program **************" << std::endl << prg;
	LOG << "*********** extensional database ***********" << std::endl;
	prg.rewrite(inc.defs);
	LOG << "************* rewritten program ************" << std::endl << prg;
	prg.check();
	if (message_printer()->hasError()) {
		throw std::runtime_error("grounding stopped because of errors");
	}
	return prg.toGround(out.domains);
}

void ground(Gringo::Output::OutputBase &out, std::vector<char const *> files, std::vector<char const *> defines) {
	using namespace Gringo;
	Scripts scripts;
	IncrementalControl inc;
	Ground::Program gPrg(prepare(inc, scripts, out, files, defines));	// Files are no passed into prepare function
	LOG << "************* intermediate program *************" << std::endl << gPrg << std::endl;
	LOG << "************* grounded program *************" << std::endl;
	if (!inc.params.empty()) { gPrg.ground(inc.params, scripts, out); }
	else { gPrg.ground(scripts, out); }
}

std::vector<std::string> split(std::string const &source, char const *delimiter = " ", bool keepEmpty = false) {
	std::vector<std::string> results;
	size_t prev = 0;
	size_t next = 0;
	while ((next = source.find_first_of(delimiter, prev)) != std::string::npos) {
		if (keepEmpty || (next - prev != 0)) { results.push_back(source.substr(prev, next - prev)); }
		prev = next + 1;
	}
	if (prev < source.size()) { results.push_back(source.substr(prev)); }
	return results;
}
std::pair<std::string, std::string> splitFirst(std::string const &source, char const *delimiter = " ") {
	size_t next = source.find_first_of(delimiter, 0);
	if (next == std::string::npos) { throw std::runtime_error("splitFirst: delimiter not found"); }
	return{ source.substr(0, next), source.substr(next + 1) };
}
void printVersion() {
	std::cout <<
		"gringo version " GRINGO_VERSION "\n"
		"\n"
		"Copyright (C) Roland Kaminski\n"
		"License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n"
		"Gringo is free software: you are free to change and redistribute it.\n"
		"There is NO WARRANTY, to the extent permitted by law.\n";
}
void printHelp() {
	printVersion();
	std::cout <<
		"\n"
		"Usage: gringo [options] [files]\n"
		"\n"
		"Gringo Options:\n"
		"\n"
		"  --text,-t        : Print plain text format\n"
		"  --lparse-rewrite : Use in conjunction with -t to inspect lparse rewriting.\n"
		"  -f <file>        : Explicitly pass a file name\n"
		"                     (e.g., file names starting with -)\n"
		"  -c <id>=<term>   : Replace term occurences of <id> with <term>\n"
		"\n"
		"Gringo Warnings:\n"
		"\n"
		"  -Wno-atom-undefined        : a :- b.\n"
		"  -Wno-define-cyclic         : #const a=b. #const b=a.\n"
		"  -Wno-define-redfinition    : #const a=1. #const a=2.\n"
		"  -Wno-file-included         : #include \"a.lp\". #include \"a.lp\".\n"
		"  -Wno-nonmonotone-aggregate : a :- #sum { 1:a; -1:a } >= 0.\n"
		"  -Wno-term-undefined        : p(1/0).\n"
		"\n"
		"Basic Options:\n"
		"\n"
		"  --help,-h    : Print help information and exit\n"
		"  --version,-v : Print version information and exit\n"
		"  --verbose,-V : Print intermediate program representations\n"
		"\n"
		"Usage: gringo [options] [files]\n";
}
int main(int argc, char **argv) {
	/*
		the names of the files are passed through argv
	*/
	using namespace Gringo;
	try {
		bool text = false;
		bool lparseRewrite = false;
		Output::OutputPredicates outPreds;
		std::vector<char const *> files;
		std::vector<char const *> defines;
		for (; argc > 1; argc--, argv++) {
			if (!strcmp(argv[1], "--lparse-rewrite")) { lparseRewrite = true; }
			else if (!strcmp(argv[1], "-t") || !strcmp(argv[1], "--text")) { text = true; }
			else if (!strcmp(argv[1], "-h") || !strcmp(argv[1], "--help")) { printHelp(); std::exit(0); }
			else if (!strcmp(argv[1], "-v") || !strcmp(argv[1], "--version")) { printVersion(); std::exit(0); }
			else if (!strcmp(argv[1], "-V") || !strcmp(argv[1], "--verbose")) { g_verbose = true; }
			// This part takes a file and puts it on the file stack.
			else if (!strcmp(argv[1], "-f") && argc > 2) { files.emplace_back(argv[2]); --argc; ++argv; }

			else if (!strcmp(argv[1], "-c") && argc > 2) { defines.emplace_back(argv[2]); --argc; ++argv; }
			else if (!strcmp(argv[1], "-Wno-define-redfinition")) { message_printer()->disable(W_DEFINE_REDEFINTION); }
			else if (!strcmp(argv[1], "-Wno-define-cyclic")) { message_printer()->disable(W_DEFINE_CYCLIC); }
			else if (!strcmp(argv[1], "-Wno-term-undefined")) { message_printer()->disable(W_TERM_UNDEFINED); }
			else if (!strcmp(argv[1], "-Wno-atom-undefined")) { message_printer()->disable(W_ATOM_UNDEFINED); }
			else if (!strcmp(argv[1], "-Wno-nonmonotone-aggregate")) { message_printer()->disable(W_NONMONOTONE_AGGREGATE); }
			else if (!strcmp(argv[1], "-Wno-file-included")) { message_printer()->disable(W_FILE_INCLUDED); }
			else if (!strncmp(argv[1], "-", 1) && strlen(argv[1]) > 1) { printHelp(); throw std::runtime_error(std::string("unknown option: ") + argv[1]); }

			// This part also takes a file and puts it on the file stack
			else { files.emplace_back(argv[1]); }
		}
		if (text) {
			Output::OutputBase out(std::move(outPreds), std::cout, lparseRewrite);
			ground(out, files, defines);
		}
		else {
			Output::PlainLparseOutputter plo(std::cout);
			Output::OutputBase out(std::move(outPreds), plo);
			ground(out, files, defines);	// The files are passed here for parsing
		}
	}
	catch (std::exception &e) {
		std::cerr << "\n" << "Exception: " << e.what() << std::endl;
		return 1;
	}
	return 0;
}

