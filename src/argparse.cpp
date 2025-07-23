#include "argparse.hpp"
#include <stdexcept>
#include <cassert>
std::optional<std::string_view> OptionSchema::pop_name(std::string_view &desc) {
	if (desc.size() == 0) {
		return std::nullopt;
	}
	size_t i = desc.find_first_of(',');
	if (i == 0) {
		throw std::logic_error(
			"Can't add empty arguments to OptionDescriptor"
		);
	}
	if (argmap.contains(desc.substr(0, i))) {
		throw std::logic_error(
			"Can't add already-added arguments to OptionDescriptor"
		);
	}
	std::string_view arg = desc.substr(0, i);
	if (i == std::string_view::npos) {
		i = desc.size() - 1;
	}
	desc.remove_prefix(i + 1);
	return arg;
}

namespace {

std::pair<std::string_view, std::optional<std::string_view>>
parse_long_opt(std::string_view opt) {
	size_t retval = opt.find_first_of('=');
	if (retval == std::string_view::npos) {
		return std::make_pair(opt, std::nullopt);
	}
	return std::make_pair(opt.substr(0, retval), opt.substr(retval + 1));
}

}

// this is awful but i don't know how to make it sane ;-;
std::optional<std::vector<std::string_view>> 
OptionSchema::parse(int argc, char **argv) {
	ArgBuf *wanted_arg = nullptr;
	bool stop_arg_parsing = false;
	std::vector<std::string_view> outvec;
	for (int i = 1; i < argc; ++i) {
		std::string_view str = argv[i];
		if (str.size() == 0) {
			return std::nullopt;
		}
		if (stop_arg_parsing) {
			outvec.push_back(str);
			continue;
		}
		if (wanted_arg != nullptr && str[0] == '-') {
			return std::nullopt;
		}
		if (wanted_arg != nullptr) {
			assert(wanted_arg->index() != 0);
			if (wanted_arg->index() == 1) {
				std::get<1>(*wanted_arg).get().push_back(str);
			}
			else {
				std::get<2>(*wanted_arg).get() = str;
			}
			wanted_arg = nullptr;
		}
		else if (str.size() == 1) {
			outvec.push_back(str); // probably a '-'
		}
		else if (str[0] == '-' && str[1] == '-' && str.size() == 2) {
			if (wanted_arg != nullptr) {
				return std::nullopt;
			}
			stop_arg_parsing = true;
		}
		else if (str[0] == '-' && str[1] == '-') {
			auto [fst, last] = parse_long_opt(str.substr(2));
			if (!argmap.contains(fst)) {
				return std::nullopt;
			}
			ArgBuf &arg = argmap.at(fst);
			if (arg.index() == 0 && last.has_value()) {
				return std::nullopt;
			}
			if (arg.index() == 0) {
				std::get<0>(arg).get() = true;
			}
			else if (!last.has_value()) {
				wanted_arg = &arg;
				continue;
			}
			else if (arg.index() == 1) {
				std::get<1>(arg).get().push_back(last.value());
			}
			else {
				std::get<2>(arg).get() = last.value();
			}
		}
		else if (str[0] == '-') {
			if (!argmap.contains(str.substr(1, 1))) {
				return std::nullopt;
			}
			ArgBuf &arg = argmap.at(str.substr(1, 1));
			if (arg.index() == 0 && !parse_short_flags(str.substr(1))) {
				return std::nullopt;
			}
			else if (arg.index() == 1 && str.size() > 2) {
				std::get<1>(arg).get().push_back(str.substr(2));
			}
			else if (arg.index() == 1) {
				wanted_arg = &arg;
			}
			else if (arg.index() == 2 && str.size() > 2) {
				std::get<2>(arg).get() = str.substr(2);
			}
			else if (arg.index() == 2) {
				wanted_arg = &arg;
			}
		}
		else {
			outvec.push_back(str);
		}
	}
	if (wanted_arg != nullptr) {
		return std::nullopt;
	}
	return outvec;
}

bool OptionSchema::parse_short_flags(std::string_view shortflags) {
	for (size_t i = 0; i < shortflags.size(); ++i) {
		if (!argmap.contains(shortflags.substr(i, 1))) {
			return false;
		}
		ArgBuf &buf = argmap.at(shortflags.substr(i, 1));
		if (buf.index() != 0) {
			return false;
		}
		std::get<0>(buf).get() = true;
	}
	return true;
}
