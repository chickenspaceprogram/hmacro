#pragma once
#include <optional>
#include <expected>
#include <string>
#include <variant>
#include <filesystem>
#include <vector>
#include "tokbuf.hpp"

class FilePos {
	public:
	using PathType = std::variant<std::filesystem::path, std::string_view>;
	FilePos() = delete;
	// Adds the given file to the TokBuf.
	static std::optional<FilePos> add_file(TokBuf &tbuf, std::filesystem::path &&pth);
	static FilePos add_inbuilt_prelude(TokBuf &tbuf);

	// if the file still exists, returns std::nullopt
	// if the file has been fully popped, returns the number of chars to pop from the next file
	std::optional<size_t> increment_pos(const TokBuf &buf, size_t num_popped);
	size_t row() const { return cur_row; }
	size_t col() const { return cur_col; }
	std::string_view get_name() const {
		if (path.index() == 0) {
			return std::get<0>(path).native();
		}
		return std::get<1>(path);
	}
	const PathType &get_path() const {
		return path;
	}

	private:

	FilePos(
		const PathType &pth,
		size_t fst_untouched_chr, size_t last_chr
	) : path(pth), 
	fst_untouched_chr(fst_untouched_chr), last_chr(last_chr) {}

	FilePos(
		PathType &&pth,
		size_t fst_untouched_chr, size_t last_chr
	) : path(std::move(pth)),
	fst_untouched_chr(fst_untouched_chr), last_chr(last_chr) {}

	PathType path;
	size_t fst_untouched_chr;
	size_t last_chr;
	size_t cur_row = 1;
	size_t cur_col = 1;
};

class FileHandler {
	public:
	using FileList = std::span<std::string_view>;
	static std::expected<FileHandler, std::string> default_prelude(FileList preludes, FileList epilogues, FileList start_files) {
		auto res = no_default_prelude(preludes, epilogues, start_files);
		if (!res.has_value()) {
			return res;
		}
		res.value().pos_stack.push_back(FilePos::add_inbuilt_prelude(res.value().tbuf));
		return res;
	}
	static std::expected<FileHandler, std::string> no_default_prelude(FileList preludes, FileList epilogues, FileList start_files);

	bool add_file(std::string_view pth);
	size_t row() const { return pos_stack.back().row(); }
	size_t col() const { return pos_stack.back().col(); };
	std::string_view filename() const { return pos_stack.back().get_name(); }

	void push_front(std::string_view str) { tbuf.push_front(str); }
	void pop_front(size_t amt);
	TokBuf::ErrCode peek_front(Token &tok, bool parse_scope) { return tbuf.peek_front(tok, parse_scope); }

	void reserve(size_t amt) { tbuf.reserve(amt); }

	size_t size() const { return tbuf.size(); }
	size_t capacity() const { return tbuf.capacity(); }
	bool empty() const { return tbuf.empty(); }

	std::string_view get_view() const { return tbuf.get_view(); }
	char operator[](size_t ind) const { return tbuf[ind]; }

	private:
	bool add_file_nonrelative(std::filesystem::path &&pth);
	std::vector<FilePos> pos_stack;
	TokBuf tbuf;
};

