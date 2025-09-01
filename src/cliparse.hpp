#pragma once
#include <concepts>
#include <string>
#include <optional>
#include <variant>

template <typename T>
concept ArgType = std::convertible_to<T, bool>
               || std::convertible_to<T, std::string_view>
               || std::convertible_to<T, std::optional<std::string_view>>;

template <typename T>
concept IsVoid = std::same_as<void, std::remove_cvref_t<T>>;

struct ArgName {
	explicit ArgName(char shortname) : shortname(shortname), longname(std::nullopt) {}
	explicit ArgName(std::string_view longname) : shortname(std::nullopt), longname(longname) {}
	explicit ArgName(char shortname, std::string_view longname) : shortname(shortname), longname(longname) {}
	explicit ArgName(std::string_view longname, char shortname) : shortname(shortname), longname(longname) {}

	std::optional<char> get_shortname() const { return shortname; }
	std::optional<std::string_view> get_longname() const { return longname; }
	private:
	std::optional<char> shortname;
	std::optional<std::string_view> longname;
};

template <ArgType T>
using ArgStruct = std::pair<T, ArgName>;


