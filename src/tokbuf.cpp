#include "tokbuf.hpp"
#include <bit>
#include <cstring>
#include <cctype>
#include <fstream>

namespace {

bool check_esc(std::string_view view) {
	return view.size() >= 2 && view[0] == '\\' && (
		view[1] == '\\' ||
		view[1] == '\n' ||
		view[1] == '$' ||
		view[1] == '{' ||
		view[1] == '}' ||
		view[1] == '[' ||
		view[1] == ']'
	);
}

std::optional<Token> try_parse_macro(std::string_view view) {
	if (
		view.size() < 2 || 
		view[0] != '\\' ||
		!(std::isalpha(view[1]) || view[1] == '-' || view[1] == '_')
	) {
		return std::nullopt;
	}
	size_t ind = 0;
	for (; ind < view.size(); ++ind) {
		if (!(std::isalnum(view[ind]) || view[ind] == '-' || view[ind] == '_')) {
			break;
		}
	}
	return std::optional(Token {.type = Token::Macro, .elem = view.substr(0, ind)});
}

std::optional<Token> try_parse_scope(std::string_view view) {
	// this is here for sanity checks
	if (
		view.size() < 2 ||
		view[0] != '{'
	) {
		return std::nullopt;
	}
	size_t nbrack = 0;
	size_t i = 0;
	do {
		size_t res = view.find_first_of("\\{}", i);
		if (res == std::string_view::npos) {
			break;
		}
		i += res;
		if (view[i] == '\\') {
			++i;
		}
		else if (view[i] == '{') {
			++nbrack;
		}
		else {
			--nbrack;
		}
	} while (nbrack != 0 && i < view.size());
	if (nbrack != 0) {
		return std::nullopt;
	}
	return std::optional(Token {.type = Token::Scope, .elem = view.substr(0, i + 1)});
}

Token parse_text(std::string_view view) {
	size_t ind = 0;
	while (true) {
		ind = view.substr(ind).find_first_of("\\]");
		if (ind == std::string_view::npos) {
			return Token {.type = Token::Txt, .elem = view};
		}
		if (check_esc(view.substr(ind))) {
			ind += 2;
		}
		else {
			return Token {.type = Token::Txt, .elem = view.substr(0, ind)};
		}
	}
}

}

std::optional<Token> TokBuf::peek_front(bool parse_scope) {
	if (empty()) {
		return std::nullopt;
	}
	if (check_esc(get_view())) {
		return std::optional(parse_text(get_view())); // escchr, parse as txt
	}
	std::optional<Token> res = try_parse_macro(get_view());
	if (res.has_value()) {
		return res;
	}
	if (parse_scope && size() >= 2 && buf[fst_index] == '{') {
		res = try_parse_scope(get_view());
		if (res.has_value()) {
			return res;
		}
		else {
			errptr = "Failed to find closing bracket";
			return std::nullopt;
		}
	}
	if (parse_scope && size() >= 1 && buf[fst_index] == '[') {
		return std::optional(Token{.type = Token::ExpScopeStart, .elem = std::string_view(&buf[fst_index], 1)});
	}
	if (size() >= 1 && buf[fst_index] == ']') {
		return std::optional(Token{.type = Token::ExpScopeEnd, .elem = std::string_view(&buf[fst_index], 1)});
	}
	return std::optional(parse_text(get_view()));
}

void TokBuf::push_front(std::string_view elem) {
	if (elem.size() == 0) {
		return;
	}
	reserve(size() + elem.size());
	fst_index -= elem.size();
	std::memcpy(&buf[fst_index], elem.data(), elem.size());
}


ErrCode TokBuf::push_file(std::string_view filname) {
	// i hate exceptions, why is the STL like this
	try {
		size_t orig_sz = size() + 1;
		std::filesystem::path pth(filname);
		if (pth.is_absolute()) {
			if (recursive_include(pth)) {
				return ErrCode::RecursiveInclude;
			}
			filnames.push_back(std::make_pair(pth, orig_sz));
		}
		else if (!have_basepath && filnames.size() == 0) {
			filnames.push_back(std::make_pair(pth, orig_sz));
		}
		else {
			std::filesystem::path dir(top_path().parent_path());
			dir / pth;
			pth = dir;
			if (recursive_include(dir)) {
				return ErrCode::RecursiveInclude;
			}
			filnames.push_back(std::make_pair(dir, orig_sz));
		}
		size_t sz = std::filesystem::file_size(pth);
		reserve(sz + size());
		std::ifstream in(pth.string());
		in.read(&buf[fst_index - sz], sz);
		size_t n_read = in.gcount(); // cast is fine, negative std::streamsize aren't used
		if (n_read != sz) {
			return ErrCode::SysErr;
		}
		fst_index -= sz;
	} catch (const std::filesystem::filesystem_error &err) {
		return ErrCode::SysErr;
	} catch (const std::ios_base::failure &fail) {
		return ErrCode::SysErr;
	}
	return ErrCode::Ok;
}

void TokBuf::reserve(size_t amt) {
	if (size() + amt <= total_size) {
		return;
	}
	size_t newsize = std::bit_ceil(size() + amt);
	std::unique_ptr<char []> tmp = std::make_unique<char []>(newsize + 1); // adding '\0'
	if (total_size != 0) {
		std::memcpy(&tmp[newsize - size()], &buf[fst_index], size() + 1);
	}
	else {
		tmp[newsize] = '\0';
	}
	fst_index = newsize - size();
	total_size = newsize;
	buf.swap(tmp);
}


bool TokBuf::recursive_include(const std::filesystem::path &path) const {
	std::error_code ec;
	if (std::filesystem::equivalent(basepath, path, ec)) {
		return true;
	}
	for (const auto &[elem, _] : filnames) {
		if (std::filesystem::equivalent(elem, path, ec)) {
			return true;
		}
	}
	return false;
}
