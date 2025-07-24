#include "cli-handler.hpp"


std::optional<MacroMap>
get_init_macromap(std::span<std::string_view> macro_defs) {
	MacroMap map;
	for (auto elem : macro_defs) {
		size_t fst_semicolon = elem.find_first_of(';');
		if (fst_semicolon == std::string_view::npos) {
			return std::nullopt;
		}
		std::string_view name = elem.substr(0, fst_semicolon);
		elem.remove_prefix(fst_semicolon + 1);
		size_t snd_semicolon = elem.find_first_of(';');
		if (snd_semicolon == std::string_view::npos) {
			return std::nullopt;
		}
		std::string_view nargs = elem.substr(0, snd_semicolon);
		elem.remove_prefix(snd_semicolon + 1);
		std::string_view exp = elem;

		if (!is_macro_name(name)) {
			return std::nullopt;
		}

		auto nargs_parsed = parse_int(nargs);
		if (!nargs_parsed.has_value()) {
			return std::nullopt;
		}

		auto exp_parsed = parse_def_args(exp);
		if (!exp_parsed.has_value()) {
			return std::nullopt;
		}

		MacroTemplate temp(
			std::move(exp_parsed.value()),
			nargs_parsed.value()
		);
		map.emplace(name, std::move(temp));
	}
	return map;
}
