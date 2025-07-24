#include <iostream>
#include <cstdlib>
#include <expected>
#include "cli-handler.hpp"
#include "filehandler.hpp"
#include "parser.hpp"

void print_version() {
	std::cerr
<< "hmacro v3.0.0, Copyright (C) 2025 Athena Boose\n\n"
<< "This program comes with ABSOLUTELY NO WARRANTY.\n"
<< "This is free software, and you are welcome to redistribute it\n"
<< "under certain conditions; see the GNU General Public License for details."
<< std::endl;
}
void print_help() {
	std::cerr
<< "Usage:\n\n"

<< "hmacro [Flags] [Options] <filenames>\n\n"

<< "Flags:\n\n"

<< "-v, --version            - Print a version message, then exit\n"
<< "-h, --help               - Print a help message, then exit\n"
<< "-l, --license            - Display information about hmacro\'s license\n"
<< "-P, --no-default-prelude - Disables the default Prelude and Epilogue.\n"
<< "-E, --keep-esc           - Disables postprocessing of escaped characters\n\n"

<< "Options:\n\n"

<< "-Dmacro;3;exp         - Defines a new macro, named \"macro\", that pops 3\n"
<< "--define=macro;3;exp    arguments from the stack and expands to \"exp\". The\n"
<< "                        macro is defined after the Prelude and before any\n"
<< "                        files are expanded.\n\n"

<< "-p <prelude>          - Adds the file <prelude> to the Prelude. All files are\n"
<< "--prelude=<prelude>     added after the default Prelude, if enabled.\n\n"

<< "-e <epilogue>         - Adds the file <epilogue> to the Epilogue. All files\n"
<< "--epilogue=<epilogue>   are added before the default Epilogue, if enabled.\n\n"

<< "-o <output>           - Writes output to the file <output>. If this option is\n"
<< "--output=<output>       not specified, output is written to stdout.\n\n"

<< "You can pass a list of filenames to hmacro.\n"
<< "They will each be expanded separately and the results concatenated.\n\n"

<< "Passing -- as an argument will result in all arguments after it being treated\n"
<< "as files to read from.\n\n"

<< "Furthermore, passing - as an argument will make hmacro read from stdin.\n\n"

<< "hmacro doesn't play super nicely in shell pipes. It works fine, but the current\n"
<< "implementation buffers input until it ends, then parses and outputs it all at\n"
<< "once."
	<< std::endl;
}
void print_license() {
	std::cerr
<< "Copyright (C) 2025 Athena Boose\n\n"

<< "This program is free software: you can redistribute it and/or modify\n"
<< "it under the terms of the GNU General Public License as published by\n"
<< "the Free Software Foundation, either version 3 of the License, or\n"
<< "(at your option) any later version.\n\n"

<< "This program is distributed in the hope that it will be useful,\n"
<< "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
<< "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
<< "GNU General Public License for more details.\n\n"

<< "You should have received a copy of the GNU General Public License\n"
<< "along with this program.  If not, see <https://www.gnu.org/licenses/>."
<< std::endl;
}


int main(int argc, char **argv) {
	auto hndl = CliHandler::parse(argc, argv);
	if (!hndl.has_value()) {
		std::cerr << "Bad CLI arguments passed; try `hmacro --help' for help" << std::endl;
		return EXIT_FAILURE;
	}
	if (hndl.value().have_something() == false) {
		std::cerr << "Try `hmacro --help' for help" << std::endl;
		return EXIT_FAILURE;
	}
	if (hndl.value().want_help) {
		print_help();
		return EXIT_SUCCESS;
	}
	if (hndl.value().want_version) {
		print_version();
		return EXIT_SUCCESS;
	}
	else if (hndl.value().want_license) {
		print_license();
		return EXIT_SUCCESS;
	}
	std::expected<FileHandler, std::string> handler;

	if (hndl.value().no_default_prelude) {
		handler = FileHandler::no_default_prelude(
			hndl.value().preludes,
			hndl.value().epilogues,
			hndl.value().files
		);
	}
	else {
		handler = FileHandler::default_prelude(
			hndl.value().preludes,
			hndl.value().epilogues,
			hndl.value().files
		);
	}
	if (!handler.has_value()) {
		std::cerr << "hmacro error on opening files:\n"
		          << handler.error()
			  << "\nTry hmacro --help"
			  << std::endl;
		return EXIT_FAILURE;
	}
	auto macro_map = get_init_macromap(hndl.value().macros);
	if (!macro_map.has_value()) {
		std::cerr << "Error: invalid predefined macro. Try hmacro --help" << std::endl;
		return EXIT_FAILURE;
	}

	ArgStack stk;

	auto result = parse(handler.value(), macro_map.value(), stk);
	if (!result.has_value()) {
		std::cerr << handler.value().filename() << ':' << handler.value().row() << ':' << handler.value().col() << ":\n";
		for (const auto &el : result.error()) {
			std::cerr << el << '\n';
		}
		stk.dbg_print(std::cerr);
		std::cerr.flush();
		return EXIT_FAILURE;
	}

	// temporary!
	//
	std::cout << remove_escs(result.value());
	std::cerr.flush();
}
