#pragma once
#include <string>
#include <vector>
#include <unordered_map>
#include <functional>
#include <variant>
#include <optional>


class OptionSchema {
	public:
	void add(std::string_view opt_desc, bool &out_have_arg) {
		process_opt_desc(opt_desc, ArgBuf(std::ref(out_have_arg)));
	}
	void add(std::string_view opt_desc, std::string_view &outarg) {
		process_opt_desc(opt_desc, ArgBuf(std::ref(outarg)));
	}
	void add(std::string_view opt_desc, std::vector<std::string_view> &outarg) {
		process_opt_desc(opt_desc, ArgBuf(std::ref(outarg)));
	}
	std::optional<std::vector<std::string_view>> 
	parse(int argc, char **argv);
	private:
	using ArgBuf = std::variant<
		std::reference_wrapper<bool>,
		std::reference_wrapper<std::vector<std::string_view>>,
		std::reference_wrapper<std::string_view>
	>;
	void process_opt_desc(std::string_view opt_desc, const ArgBuf &buf) {
		std::optional<std::string_view> opt = pop_name(opt_desc);
		while (opt.has_value()) {
			argmap.emplace(opt.value(), buf);
			opt = pop_name(opt_desc);
		}
	}
	bool parse_short_flags(std::string_view shortflags);
	
	std::optional<std::string_view> pop_name(std::string_view &desc);
	std::unordered_map<std::string_view, ArgBuf> argmap;

};

