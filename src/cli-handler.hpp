#pragma once

#include "argparse.hpp"
#include "parser.hpp"

struct CliHandler {
	static std::optional<CliHandler> parse(int argc, char **argv) {
		CliHandler handle;
		OptionSchema schema;
		schema.add("h,help", handle.want_help);
		schema.add("v,version", handle.want_version);
		schema.add("l,license", handle.want_license);
		schema.add("D,define", handle.macros);
		schema.add("p,prelude", handle.preludes);
		schema.add("e,epilogue", handle.epilogues);
		schema.add("P,no-default-prelude", handle.no_default_prelude);
		schema.add("E,keep-esc", handle.keep_esc);
		auto res = schema.parse(argc, argv);
		if (!res.has_value()) {
			return std::nullopt;
		}
		handle.files = res.value();
		return handle;
	}
	bool have_something() const {
		return 	want_help || 
			want_version || 
			want_license || 
			macros.size() != 0 || 
			preludes.size() != 0 || 
			epilogues.size() != 0 ||
			no_default_prelude || 
			keep_esc || 
			files.size() != 0
		;
	}
	bool want_help = false;
	bool want_version = false;
	bool want_license = false;
	std::vector<std::string_view> macros;
	std::vector<std::string_view> preludes;
	std::vector<std::string_view> epilogues;
	bool no_default_prelude = false;
	bool keep_esc = false;
	std::vector<std::string_view> files;
	private:
	CliHandler() {}
};

std::optional<MacroMap>
get_init_macromap(std::span<std::string_view> macro_defs);
