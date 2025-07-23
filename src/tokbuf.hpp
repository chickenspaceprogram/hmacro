#pragma once
#include <cstddef>
#include <string>
#include <filesystem>
#include <optional>
#include <memory>
#include <vector>

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

enum class ErrCode {
	SysErr,
	RecursiveInclude,
	Ok,
};

class TokBuf {
	public:
	TokBuf() : have_basepath(false) {}
	explicit TokBuf(std::filesystem::path &&filname) 
		: have_basepath(true), basepath(std::move(filname)) {}
	explicit TokBuf(const std::filesystem::path &filname) 
		: have_basepath(true), basepath(filname) {}
	// token is only valid until push_front or reserve are called
	std::optional<Token> peek_front(bool parse_scope);
	void pop_front(size_t amt) {
		fst_index += amt;
		if (size() < top_size()) {
			filnames.pop_back();
		}
	}
	// pushes text to the front of the TokBuf
	//
	// the text is considered to be part of whatevfer file was most recently
	// added to the TokBuf
	void push_front(std::string_view elem);

	// pushes text from a file to the front of the TokBuf
	//
	// the text must have elem.size() > 0 for the file to be added to the TokBuf
	ErrCode push_file(std::string_view filname);

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
	// gets a (potentially null) pointer to the error
	const char *get_error() const { return errptr; }

	private:
	bool recursive_include(const std::filesystem::path &path) const;
	const std::filesystem::path &top_path() const {
		if (filnames.size() == 0) {
			return basepath;
		}
		else {
			return filnames.back().first;
		}
	}
	size_t top_size() const {
		if (filnames.size() == 0) {
			return 0;
		}
		return filnames.back().second;
	}


	std::unique_ptr<char []> buf;
	bool have_basepath;
	std::filesystem::path basepath;
	std::vector<std::pair<std::filesystem::path, size_t>> filnames;
	size_t fst_index = 0;
	size_t total_size = 0;
	// cringe i know
	const char *errptr = nullptr; // this will be made to point to a static string
};

