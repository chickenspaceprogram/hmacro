#include <iostream>
#include <cstdlib>
#include "cli-handler.hpp"


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
}
