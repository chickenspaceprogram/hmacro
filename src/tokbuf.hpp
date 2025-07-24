#pragma once
#include <cstddef>
#include <optional>
#include <memory>
#include <cassert>
#include <istream>

struct Token {
	enum {
		Macro,
		Scope,
		ExpScopeStart,
		ExpScopeEnd,
		Txt,
	} type;
	std::string_view elem;
};

class TokBuf {
	public:
	enum ErrCode {
		Ok,
		NoTok,
		Err,
	};
	TokBuf() {}
	// token is only valid until push_front or reserve are called
	// If ErrCode == Ok:
	// 	tok.type is type of token
	// 	tok.elem is text of token
	// If ErrCode == NoTok:
	// 	tok is unmodified
	// If ErrCode == Err:
	// 	tok.elem is an error string
	ErrCode peek_front(Token &tok, bool parse_scope) const;
	void pop_front(size_t amt) {
		fst_index += amt;
	}
	// pushes text to the front of the TokBuf
	//
	// the text is considered to be part of whatevfer file was most recently
	// added to the TokBuf
	void push_front(std::string_view elem);

	// Reads `num_to_push` bytes from a std::istream.
	//
	// If successful, returns true.
	// If unsuccessful, returns false.
	bool push_from_istream(std::istream &stream, size_t num_to_push);


	// reserves at least `amt` space in the buffer
	//
	// if `amt` is less than what is already in the buffer, does nothing
	void reserve(size_t amt);

	// same as STL
	size_t size() const { return total_size - fst_index; }
	size_t capacity() const { return total_size; }
	bool empty() const { return size() == 0; }

	// gets the char in the buffer at index `ind`
	char operator[](size_t ind) const { return buf[fst_index + ind]; }
	// gets a view onto the entire contents of the buffer
	//
	// this view is only valid so long as neither push_front nor pop_front
	// nor reserve are called
	std::string_view get_view() const { return std::string_view(&buf[fst_index], size()); }

	private:

	std::unique_ptr<char []> buf;
	size_t fst_index = 0;
	size_t total_size = 0;
};

std::string remove_escs(std::string_view view);
