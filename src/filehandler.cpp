#include "filehandler.hpp"
#include "tokbuf.hpp"
#include <cassert>
#include <fstream>
#include <expected>


namespace {

// contains (num newlines, chrs after last newline)
std::pair<size_t, size_t> get_nls(std::string_view view) {
	size_t nl_loc = view.find_first_of('\n');
	size_t fst_remaining_chr = 0;
	size_t num_nls = 0;
	while (nl_loc != std::string_view::npos) {
		++num_nls;
		fst_remaining_chr = nl_loc + 1;
		nl_loc = view.find_first_of('\n');
	}
	return std::make_pair(num_nls, view.size() - fst_remaining_chr);
}

const char PRELUDE[] = {
#include "prelude.xxd.hm"
};

const std::string_view PRELUDE_VIEW = PRELUDE;

}

std::optional<FilePos> FilePos::add_file(TokBuf &tbuf, std::filesystem::path &&pth) {
	size_t start_sz = tbuf.size();
	std::ifstream stream(pth);
	std::error_code ec;
	uintmax_t filesize = std::filesystem::file_size(pth, ec);
	if (!ec || filesize == static_cast<uintmax_t>(-1)) {
		return std::nullopt;
	}
	if (!tbuf.push_from_istream(stream, filesize)) {
		return std::nullopt;
	}
	size_t end_sz = tbuf.size();
	return FilePos(
		pth,
		end_sz,
		start_sz + 1
	);
}
FilePos FilePos::add_inbuilt_prelude(TokBuf &tbuf) {
	size_t start_sz = tbuf.size();
	tbuf.push_front(PRELUDE_VIEW);
	size_t end_sz = tbuf.size();
	return FilePos(std::string_view("Default Prelude"), end_sz, start_sz + 1);
}

std::optional<size_t> FilePos::increment_pos(const TokBuf &buf, size_t num_popped) {
	assert(buf.size() >= last_chr);
	// indexing from the back; last char of buf is at index 1
	size_t last_chr_unpopped = buf.size() - num_popped;
	if (last_chr_unpopped >= fst_untouched_chr) {
		return std::nullopt;
	}
	if (last_chr_unpopped < last_chr) {
		// entire file popped
		return last_chr - last_chr_unpopped - 1;
	}
	size_t fst_chr_of_file = buf.size() - fst_untouched_chr;
	size_t nchrs_popped_from_file = fst_untouched_chr - last_chr_unpopped;
	auto [nls, chrs_after_nl] = get_nls(buf.get_view().substr(fst_chr_of_file, nchrs_popped_from_file));
	cur_row += nls;
	if (nls == 0) {
		cur_col += chrs_after_nl;
	}
	else {
		cur_col = 1 + chrs_after_nl;
	}
	fst_untouched_chr = last_chr_unpopped;

	// invariant - first character must be further back than last one
	assert(fst_untouched_chr >= last_chr);
	return std::nullopt;
}

std::expected<FileHandler, std::string> FileHandler::no_default_prelude(FileList preludes, FileList epilogues, FileList start_files) {
	FileHandler buf;
	try {
		for (auto it = epilogues.rbegin(); it == epilogues.rend(); ++it) {
			if (!buf.add_file_nonrelative(*it)) {
				return std::unexpected("Error opening file: " + std::string(*it));
			}
		}
		for (auto it = start_files.rbegin(); it == start_files.rend(); ++it) {
			if (!buf.add_file_nonrelative(*it)) {
				return std::unexpected("Error opening file: " + std::string(*it));
			}
		}
		for (auto it = preludes.rbegin(); it == preludes.rend(); ++it) {
			if (!buf.add_file_nonrelative(*it)) {
				return std::unexpected("Error opening file: " + std::string(*it));
			}
		}
	}
	catch (const std::filesystem::filesystem_error &e) {
		return std::unexpected(std::string(e.what()));
	}
	return buf;
}

bool FileHandler::add_file(std::string_view pth) {
	if (pos_stack.size() == 0 || pos_stack.back().get_path().index() == 1) {
		add_file_nonrelative(pth);
	}
	try {
		std::filesystem::path base = 
			std::get<0>(pos_stack.back().get_path()).parent_path();
		base /= pth;
		return add_file_nonrelative(std::move(base));
	}
	catch (const std::filesystem::filesystem_error &e) {
		return false;
	}
}

bool FileHandler::add_file_nonrelative(std::filesystem::path &&pth) {
	auto fil = FilePos::add_file(tbuf, std::move(pth));
	if (!fil.has_value()) {
		return false;
	}
	pos_stack.push_back(fil.value());
	return true;
}

void FileHandler::pop_front(size_t amt) {
	auto res = pos_stack.back().increment_pos(tbuf, amt);
	while (res.has_value()) {
		pos_stack.pop_back();
		res = pos_stack.back().increment_pos(tbuf, res.value());
	}
}
