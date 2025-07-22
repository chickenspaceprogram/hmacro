#pragma once

#include <optional>
#include <string>
#include <cstdint>
#include <cassert>
#include <span>
#include <vector>
#include <algorithm>

namespace arg_parser {
// i might reuse this in other projects, idk

struct LongOpt {
	enum Arg : uint8_t {
		NoArg,
		OptionalArg,
		RequiredArg,
	};
	explicit LongOpt(char shortname) 
		: name(std::nullopt), arg(NoArg), short_arg(shortname) {}
	static LongOpt no_arg(std::string_view name) {
		assert(name.find_first_of('=') == std::string::npos);
		return LongOpt(std::string(name), NoArg, std::nullopt);
	}
	static LongOpt no_arg(std::string_view name, char shortname) {
		assert(shortname != '-');
		assert(name.find_first_of('=') == std::string::npos);
		return LongOpt(std::string(name), NoArg, shortname);
	}
	static LongOpt opt_arg(std::string_view name) {
		assert(name.find_first_of('=') == std::string::npos);
		return LongOpt(std::string(name), OptionalArg, std::nullopt);
	}
	static LongOpt opt_arg(std::string_view name, char shortname) {
		assert(shortname != '-');
		assert(name.find_first_of('=') == std::string::npos);
		return LongOpt(std::string(name), OptionalArg, shortname);
	}
	static LongOpt req_arg(std::string_view name) {
		assert(name.find_first_of('=') == std::string::npos);
		return LongOpt(std::string(name), RequiredArg, std::nullopt);
	}
	static LongOpt req_arg(std::string_view name, char shortname) {
		assert(shortname != '-');
		assert(name.find_first_of('=') == std::string::npos);
		return LongOpt (std::string(name), RequiredArg, std::optional(shortname));
	}

	bool operator==(const LongOpt &lopt) const {
		if (name.has_value() != lopt.name.has_value()) {
			return false;
		}
		if (short_arg.has_value() != lopt.short_arg.has_value()) {
			return false;
		}
		if (arg != lopt.arg) {
			return false;
		}
		if (name.has_value() && name.value() != lopt.name.value()) {
			return false;
		}
		if (short_arg.has_value() && short_arg.value() != lopt.short_arg.value()) {
			return false;
		}
		return true;
	}
	bool operator==(char ch) const {
		if (!short_arg.has_value()) {
			return false;
		}
		return ch == short_arg.value();
	}
	bool operator==(std::string_view txt) const {
		if (!name.has_value()) {
			return false;
		}
		return name.value() == txt;
	}

	Arg have_arg() const { return arg; }

	private:
	LongOpt(
		std::optional<std::string> name, 
		Arg arg,
		std::optional<char> short_arg
	) : name(std::move(name)), arg(arg), short_arg(short_arg) {}

	std::optional<std::string> name;
	Arg arg;
	std::optional<char> short_arg;
};

class ArgParser {
	public:
	ArgParser() = delete;
	ArgParser(int argc, const char **argv, std::string_view shortopts) : argc(argc), argv(argv) {
		std::transform(
			shortopts.begin(), shortopts.end(), 
			std::back_inserter(longopts), 
			[](char ch){ return LongOpt(ch); }
		);
	}
	ArgParser(int argc, const char **argv, std::span<LongOpt> lopts) : argc(argc), argv(argv) {
		std::copy(
			lopts.begin(), lopts.end(),
			std::back_inserter(longopts)
		);
	}
	private:
	ArgParser(int argc, const char **argv, std::vector<LongOpt> &&longopts)
		: argc(argc), argv(argv), longopts(std::move(longopts)) {}
	int argc;
	const char **argv;
	std::vector<LongOpt> longopts;
};


}

