#include "tokbuf.hpp"
#include <bit>
#include <cstring>
#include <cctype>

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

TokBuf::ErrCode TokBuf::peek_front(Token &tok, bool parse_scope) const {
	if (empty()) {
		return ErrCode::NoTok;
	}
	if (check_esc(get_view())) {
		tok = parse_text(get_view());
		return ErrCode::Ok; // escchr, parse as txt
	}
	std::optional<Token> res = try_parse_macro(get_view());
	if (res.has_value()) {
		tok = res.value();
		return ErrCode::Ok;
	}
	if (parse_scope && size() >= 2 && buf[fst_index] == '{') {
		res = try_parse_scope(get_view());
		if (res.has_value()) {
			tok = res.value();
			return ErrCode::Ok;
		}
		else {
			tok.elem = "Failed to find closing bracket";
			return ErrCode::Err;
		}
	}
	if (parse_scope && size() >= 1 && buf[fst_index] == '[') {
		tok.type = Token::ExpScopeStart;
		tok.elem = std::string_view(&buf[fst_index], 1);
		return ErrCode::Ok;
	}
	if (size() >= 1 && buf[fst_index] == ']') {
		tok.type = Token::ExpScopeEnd;
		tok.elem = std::string_view(&buf[fst_index], 1);
		return ErrCode::Ok;
	}
	tok = parse_text(get_view());
	return ErrCode::Ok;
}

void TokBuf::push_front(std::string_view elem) {
	if (elem.size() == 0) {
		return;
	}
	reserve(size() + elem.size());
	fst_index -= elem.size();
	std::memcpy(&buf[fst_index], elem.data(), elem.size());
}

bool TokBuf::push_from_istream(std::istream &stream, size_t num_to_push) {
	if (num_to_push == 0) {
		return true;
	}
	reserve(size() + num_to_push);
	fst_index -= num_to_push;
	stream.read(&buf[fst_index], num_to_push);
	return num_to_push == (size_t)stream.gcount();
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



