#pragma once
#include <cstddef>
#include <string>
#include <optional>
#include <memory>

struct Token {
	enum {
		Macro,
		Scope,
		Txt,
	} type;
	std::string_view elem;
};

class TokBuf {
	// token is only valid until push_front or reserve are called
	std::optional<Token> peek_front(bool parse_scope);
	void pop_front(size_t amt) { fst_index += amt; }
	void push_front(std::string_view elem);
	void reserve(size_t amt);
	size_t size() const { return total_size - fst_index; }
	size_t capacity() const { return total_size; }
	bool empty() const { return size() == 0; }
	char operator[](size_t ind) const { return buf[fst_index + ind]; }
	std::string_view get_view() const { return std::string_view(&buf[fst_index], size()); }
	std::string_view get_error() const { return std::string_view(errptr); }

	private:

	std::unique_ptr<char []> buf;
	size_t fst_index = 0;
	size_t total_size = 0;
	// cringe i know
	const char *errptr = nullptr; // this will be made to point to a static string
};

