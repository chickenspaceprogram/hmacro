#include "tokbuf.hpp"
#include <bit>
#include <cstring>
#include <cctype>

namespace {

bool check_esc(std::string_view view) {
	return view.size() >= 2 && view[0] == '\\' && (
		view[1] == '\\' ||
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
	size_t ind = 2;
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
		if (view[res] == '\\') {
			++i;
		}
		else if (view[res] == '{') {
			++nbrack;
		}
		else {
			--nbrack;
		}
		i = res + 1;
	} while (nbrack != 0 && i < view.size());
	if (nbrack != 0) {
		return std::nullopt;
	}
	return std::optional(Token {.type = Token::Scope, .elem = view.substr(0, i)});
}

Token parse_text(std::string_view view) {
	size_t ind = 0;
	while (true) {
		ind = view.find_first_of("\\]", ind);
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
	if (get_view().size() >= 1 && get_view()[0] == '\\') {
		std::optional<Token> res = try_parse_macro(get_view());
		if (res.has_value()) {
			tok = res.value();
			return ErrCode::Ok;
		}
		tok.elem = "Invalid macro call";
		return ErrCode::Err;
	}
	if (parse_scope && size() >= 2 && buf[fst_index] == '{') {
		std::optional<Token> res = try_parse_scope(get_view());
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
	size_t last_bslash = elem.find_last_of('\\');
	if (elem.find_first_not_of(" \n\t\v", last_bslash + 1) == std::string_view::npos) {
		elem.remove_suffix(elem.size() - last_bslash);
		std::string_view buf_view(buf.get() + fst_index, total_size - fst_index);
		size_t fst_non_wspace = buf_view.find_first_not_of(" \n\t\v");
		if (fst_non_wspace == std::string_view::npos) {
			fst_non_wspace = size();
		}
		pop_front(fst_non_wspace);

		last_bslash = elem.find_last_of('\\');
	}
	while (last_bslash != std::string_view::npos) {
		size_t end_nl_esc = elem.find_first_not_of(" \n\t\v", last_bslash + 1);
		if (end_nl_esc == last_bslash + 1) {
			push_front_unsafe(elem.substr(last_bslash));
		}
		else if (end_nl_esc != std::string_view::npos) {
			push_front_unsafe(elem.substr(end_nl_esc));
		}
		elem.remove_suffix(elem.size() - last_bslash);
		last_bslash = elem.find_last_of('\\');
	}
	push_front_unsafe(elem);
}

void TokBuf::push_front_unsafe(std::string_view elem) {
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
	std::unique_ptr<char []> tmp_buf = std::make_unique<char []>(num_to_push);
	stream.read(tmp_buf.get(), num_to_push);
	if (num_to_push == (size_t)stream.gcount()) {
		std::string_view view(tmp_buf.get(), num_to_push);
		push_front(view);
		return true;
	}
	return false;
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


std::string remove_escs(std::string_view view) {
	std::string outbuf;
	size_t esc_loc = view.find_first_of('\\');
	while (esc_loc != std::string_view::npos) {
		outbuf += view.substr(0, esc_loc);
		view.remove_prefix(esc_loc);
		if (check_esc(view)) {
			outbuf += view[1];
			view.remove_prefix(2);
		}
		else {
			outbuf += view.substr(0, 2);
			view.remove_prefix(view.size() >= 2 ? 2 : view.size());
		}
		esc_loc = view.find_first_of('\\');
	}
	outbuf += view;
	return outbuf;
}

