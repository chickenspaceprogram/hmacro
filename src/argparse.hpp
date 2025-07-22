#pragma once

#include <string>
#include <cassert>
#include <vector>
#include <optional>
#include <algorithm>
#include <expected>

struct Flag {
	enum ArgState {
		NoArg,
		OptArg,
		ReqArg,
	};
	Flag(ArgState state, char shortname) :
		shortname(shortname), 
		longname(std::nullopt), 
		has_arg(state) {}
	Flag(ArgState state, std::string_view longname) : 
		shortname(std::nullopt),
		longname(longname), 
		has_arg(state) {}
	Flag(ArgState state, char shortname, std::string_view longname) : 
		shortname(shortname),
		longname(longname), 
		has_arg(state) {}
	bool operator==(char shortnm) const {
		return shortname == shortnm;
	}
	bool operator==(std::string_view str) const {
		return longname == str;
	}
	bool operator==(const std::string &str) const {
		return *this == std::string_view(str);
	}
	bool operator==(const char *str) const {
		return *this == std::string_view(str);
	}
	bool operator==(const Flag &flag) const {
		return shortname == flag.shortname && longname == flag.longname;
	}
	std::optional<char> shortname;
	std::optional<std::string_view> longname;
	ArgState has_arg;
};

class ArgMap {
	public:
	using ArgsType = std::vector<
		std::pair<Flag, std::optional<std::string_view>>
	>;
	ArgMap() = delete;
	explicit ArgMap(ArgsType &&args) : args(std::move(args)) {}
	bool have_flag(char shortname) {
		for (const auto &[flag, _] : args) {
			if (flag == shortname) {
				return true;
			}
		}
		return false;
	}
	bool have_flag(std::string_view longname) {
		for (const auto &[flag, _] : args) {
			if (flag == longname) {
				return true;
			}
		}
		return false;
	}
	std::optional<std::string_view> get_arg(char shortname) {
		for (const auto &[flag, str] : args) {
			if (flag == shortname) {
				return str;
			}
		}
		return std::nullopt;
	}
	std::optional<std::string_view> get_arg(std::string_view longname) {
		for (const auto &[flag, str] : args) {
			if (flag == longname) {
				return str;
			}
		}
		return std::nullopt;
	}
	private:
	ArgsType args;
};

class ArgSchema {
	public:
	void add_flag(char shortname) {
		assert(find_flag(shortname) == nullptr);
		flags.push_back(Flag(Flag::NoArg, shortname));
	}
	void add_flag(std::string_view longname) {
		assert(find_flag(longname) == nullptr);
		flags.push_back(Flag(Flag::NoArg, longname));
	}
	void add_flag(char shortname, std::string_view longname) {
		assert(find_flag(shortname) == nullptr);
		assert(find_flag(longname) == nullptr);
		flags.push_back(Flag(Flag::NoArg, shortname, longname));
	}

	void add_opt_arg(char shortname) {
		assert(find_flag(shortname) == nullptr);
		flags.push_back(Flag(Flag::OptArg, shortname));
	}
	void add_opt_arg(std::string_view longname) {
		assert(find_flag(longname) == nullptr);
		flags.push_back(Flag(Flag::OptArg, longname));
	}
	void add_opt_arg(char shortname, std::string_view longname) {
		assert(find_flag(shortname) == nullptr);
		assert(find_flag(longname) == nullptr);
		flags.push_back(Flag(Flag::OptArg, shortname, longname));
	}

	void add_req_arg(char shortname) {
		assert(find_flag(shortname) == nullptr);
		flags.push_back(Flag(Flag::ReqArg, shortname));
	}
	void add_req_arg(std::string_view longname) {
		assert(find_flag(longname) == nullptr);
		flags.push_back(Flag(Flag::ReqArg, longname));
	}
	void add_req_arg(char shortname, std::string_view longname) {
		assert(find_flag(shortname) == nullptr);
		assert(find_flag(longname) == nullptr);
		flags.push_back(Flag(Flag::ReqArg, shortname, longname));
	}
	void add(const Flag &flag) {
		assert(flag.shortname.has_value() ? 
			find_flag(flag.shortname.value()) == nullptr : 1
			&& "Short flag already in argschema!");
		assert(flag.longname.has_value() ? find_flag(flag.longname.value()) == nullptr : 1);
		assert(flag.shortname.has_value() + flag.longname.has_value() >= 1);
		flags.push_back(flag);
	}
	void add(Flag &&flag) {
		flags.push_back(std::move(flag));
	}
	using RetType = std::expected<std::pair<ArgMap, std::vector<std::string_view>>, std::string>;

	RetType parse(int argc, char **argv) const {
		ArgMap::ArgsType args;
		std::vector<std::string_view> leftovers;
		for (int i = 1; i < argc; ++i) {
			if (argv[i][0] == '-' && argv[i][1] == '-') {
				// long
			}
			else if (argv[i][0] == '-') {
				for (auto ch : std::string_view(argv[i] + 1)) {
					
				}
				// short
			}
			else {
				leftovers.push_back(argv[i]);
			}
		}

		return std::make_pair(ArgMap(std::move(args)), leftovers);
	}

	private:
	friend class ShortArgInfo;
	struct ShortArgInfo {
		enum Type {
			Options,
			OptWithArg,
			OptNeedsArg,
			OptMayHaveArg,
			NotShortArg,
			Err,
		};
		ShortArgInfo() = delete;
		ShortArgInfo(std::string_view view, const ArgSchema &sch) {
			if (view.size() < 2) {
				type = NotShortArg;
				return;
			}
			if (view[0] != '-' || view[1] == '-') {
				type = NotShortArg;
				return;
			}
			if (view.size() == 2) {
				const Flag *flg = sch.find_flag(view[1]);
				if (flg == nullptr) {
					type = Err;
					txt = "Invalid short option";
					optchr = view[1];
					return;
				}
				if (flg->has_arg == Flag::OptArg) {
					type = OptMayHaveArg;
					optchr = view[1];
				}
				else if (flg->has_arg == Flag::ReqArg) {
					type = OptNeedsArg;
					optchr = view[1];
				}
				else {
					type = Options;
					txt = view.substr(1);
				}
				return;
			}

			
		}
		Type type;
		char optchr;
		std::string_view txt;
	};
	bool validate_flags(std::string_view flags) const {
	}
	const Flag *find_flag(char ch) const {
		auto retit = std::find(flags.begin(), flags.end(), ch);
		if (retit == flags.end()) {
			return nullptr;
		}
		return &*retit;
	}
	const Flag *find_flag(std::string_view st) const {
		auto retit = std::find(flags.begin(), flags.end(), st);
		if (retit == flags.end()) {
			return nullptr;
		}
		return &*retit;
	}
	std::vector<Flag> flags;
};

