#include <array>
#include <charconv>
#include <algorithm>
#include "parser.hpp"



namespace {

struct MacroCtx {
	ArgStack &argstack;
	MacroMap &map;
	FileHandler &buf;
};

using RetType = std::expected<std::string, ErrType>;
using MacroFn = RetType (*)(MacroCtx ctx);

RetType def_fn(MacroCtx ctx) {
	if (!is_macro_name(ctx.argstack[0])) {
		ErrType es;
		es.push_back("Invalid macro name provided to \\def");
		return std::unexpected(es);
	}
	std::string_view num_view = ctx.argstack[1];
	auto nargs = parse_int(num_view);
	if (!nargs.has_value()) {
		ErrType es;
		es.push_back("Non-numeric value provided as argument $1 of \\def");
		return std::unexpected(es);
	}
	auto defargs(parse_def_args(ctx.argstack[2]));
	if (!defargs.has_value()) {
		return std::unexpected(defargs.error());
	}
	ctx.map.emplace(std::move(ctx.argstack[0]), MacroTemplate(std::move(defargs.value()), nargs.value()));
	return "";
}

RetType include_fn(MacroCtx ctx) {
	if (!ctx.buf.add_file(ctx.argstack[0])) {
		ErrType es;
		std::string st = "Error when including file `";
		st += ctx.argstack[0];
		st += "\'";
		es.push_back(st);
		return std::unexpected(es);
	}
	return "";
}

RetType if_fn(MacroCtx ctx) {
	std::string_view st = ctx.argstack[0];
	auto res = parse_int(st);
	if (!res.has_value()) {
		ErrType es;
		es.push_back("Failed to parse integer argument to macro \\if");
		return std::unexpected(es);
	}
	if (res.value()) {
		ctx.buf.push_front(ctx.argstack[1]);
	}
	else {
		ctx.buf.push_front(ctx.argstack[2]);
	}
	return "";
}

std::string num_to_argno(size_t num) {
	std::string el = "$";
	el += num;
	return el;
}

RetType err_fn(MacroCtx ctx) {
	ErrType es;
	es.push_back("\\error macro found, dumping argument stack");
	for (size_t i = 0; i < ctx.argstack.size(); ++i) {
		std::string el = num_to_argno(i);
		el += ": `";
		el += ctx.argstack[i];
		el += '\'';
		es.push_back(el);
	}
	return std::unexpected(es);
}

const std::array<std::pair<std::string_view, std::pair<size_t, MacroFn>>, 26> INBUILT_MACRO_ARR = {
	std::make_pair("def", std::make_pair(3, def_fn)),
	std::make_pair("undef", std::make_pair(1, [](MacroCtx ctx) -> RetType {
		ctx.map.erase(std::string(ctx.argstack[0]));
		return "";
	})),
	std::make_pair("include", std::make_pair(1, include_fn)),
	std::make_pair("if", std::make_pair(3, if_fn)),
	std::make_pair("error", std::make_pair(0, err_fn)),
	std::make_pair("defined", std::make_pair(1, nullptr)),
	std::make_pair("size", std::make_pair(0, nullptr)),
	std::make_pair("suffix", std::make_pair(2, nullptr)),
	std::make_pair("prefix", std::make_pair(2, nullptr)),
	std::make_pair("len", std::make_pair(1, nullptr)),
	std::make_pair("replace", std::make_pair(3, nullptr)),
	std::make_pair("reverse", std::make_pair(1, nullptr)),
	std::make_pair("find", std::make_pair(2, nullptr)),
	std::make_pair("rfind", std::make_pair(2, nullptr)),
	std::make_pair("findany", std::make_pair(2, nullptr)),
	std::make_pair("rfindany", std::make_pair(2, nullptr)),
	std::make_pair("findnone", std::make_pair(2, nullptr)),
	std::make_pair("rfindnone", std::make_pair(2, nullptr)),
	std::make_pair("eq", std::make_pair(2, nullptr)),
	std::make_pair("streq", std::make_pair(2, nullptr)),
	std::make_pair("gt", std::make_pair(2, nullptr)),
	std::make_pair("nand", std::make_pair(2, nullptr)),
	std::make_pair("isnum", std::make_pair(1, nullptr)),
	std::make_pair("add", std::make_pair(2, nullptr)),
	std::make_pair("mult", std::make_pair(2, nullptr)),
	std::make_pair("div", std::make_pair(2, nullptr)),
};
const std::unordered_map<std::string_view, std::pair<size_t, MacroFn>> 
	INBUILT_MACRO_MAP(INBUILT_MACRO_ARR.begin(), INBUILT_MACRO_ARR.end());

}

std::expected<std::vector<MacroTemplateElem>, ErrType> 
parse_def_args(std::string_view sub) {
	std::vector<MacroTemplateElem> argvec;
	while (sub.size() > 0) {
		size_t i = sub.find_first_of("$\\");
		if (i == std::string_view::npos) {
			argvec.push_back(MacroTemplateElem(std::string(sub)));
			return argvec;
		}
		if (sub[i] == '\\') {
			argvec.push_back(MacroTemplateElem(std::string(sub.substr(0, i + 2))));
			// removing '\\'
			sub.remove_prefix(i + 1);
			if (sub.size() != 0) {
				// only remove escd chr when safe
				sub.remove_prefix(1);
			}
			continue;
		}
		if (i != 0) {
			argvec.push_back(MacroTemplateElem(std::string(sub.substr(0, i))));
			sub.remove_prefix(i + 1);
		}
		auto result(parse_int(sub));
		if (!result.has_value()) {
			ErrType es;
			es.push_back("Expected numeric characters to follow `$'");
			return std::unexpected(es);
		}
		else {
			argvec.push_back(MacroTemplateElem(result.value()));
		}
	}
	return argvec;
}

std::optional<size_t> parse_int(std::string_view &view) {
	size_t start = 0;
	auto [ptr, _] = std::from_chars(view.data(), view.data() + view.size(), start);
	if (ptr == view.data()) {
		return std::nullopt;
	}
	view.remove_prefix(ptr - view.data());
	return std::optional(start);
}

std::expected<std::string, ErrType> 
MacroTemplate::expand(ArgStack &args) const {
	std::string out;
	for (const auto &elem : argslist) {
		if (elem.index() == 0) {
			out += std::get<0>(elem);
		}
		else if (args.size() < std::get<1>(elem)) {
			ErrType es;
			es.push_back("Stack underflow: Attempted to access value `" + num_to_argno(std::get<1>(elem)) + "\', which was not present in the stack");
			return std::unexpected(es);
		}
		else {
			out += args[std::get<1>(elem)];
		}
	}
	args.pop(nargs);
	return out;
}

std::expected<std::string, ErrType> 
expand_macro(
	const std::string &name, 
	ArgStack &args,
	MacroMap &map,
	FileHandler &buf
) {
	assert(name[0] == '\\');
	std::string name_without_fst = name.substr(1);
	if (INBUILT_MACRO_MAP.contains(name_without_fst)) {
		auto [minargs, fn] = INBUILT_MACRO_MAP.at(name_without_fst);
		if (args.size() < minargs) {
			ErrType es;
			std::string errtxt = "Too few arguments provided to inbuilt macro `" + name + "\'; ";
			errtxt += std::to_string(minargs);
			errtxt += " required, ";
			errtxt += std::to_string(args.size());
			errtxt += " provided";
			es.push_back(errtxt);
			return std::unexpected(es);
		}
		auto res = fn(MacroCtx{.argstack = args, .map = map, .buf = buf});
		args.pop(minargs);
		return res;
	}
	if (!map.contains(name_without_fst)) {
		ErrType evec;
		std::string str = "Unknown macro `";
		str += name;
		str += "\'";
		evec.push_back(str);
		return std::unexpected(evec);
	}
	return map.at(name_without_fst).expand(args);
}

enum class ParserState {
	Default,
	ParsingScope,
};

std::expected<std::string, ErrType>
parse(FileHandler &buf, MacroMap &map, ArgStack &stk) {
	ParserState state = ParserState::Default;
	Token tok;
	TokBuf::ErrCode ec = buf.peek_front(tok, false);
	std::string outbuf;

	std::string macname;
	std::vector<std::string> args;
	while (ec == TokBuf::Ok) {
		if (state == ParserState::Default) {
			if (tok.type == Token::ExpScopeEnd) {
				return outbuf;
			}
			if (tok.type == Token::Macro) {
				state = ParserState::ParsingScope;
				macname = tok.elem;
				assert(macname.size() > 0);
			}
			else {
				outbuf += tok.elem;
			}
			buf.pop_front(tok.elem.size());
		}
		else if (state == ParserState::ParsingScope) {
			if (tok.type == Token::Scope) {
				args.push_back(std::string(tok.elem.substr(1, tok.elem.size() - 2)));
				buf.pop_front(tok.elem.size());
			}
			else if (tok.type == Token::ExpScopeStart) {
				buf.pop_front(1);
				auto result = parse(buf, map, stk);
				if (!result.has_value()) {
					return result;
				}
				if (buf.size() == 0) {
					ErrType es;
					es.push_back("Failed to find matching `]'");
					return std::unexpected(es);
				}
				buf.pop_front(1);
				args.push_back(std::move(result.value()));
			}
			else {
				// this is fine even if we find a `]', just
				// expand the current macro, and then loop
				// back and pop off the buf until we get the `]'.

				state = ParserState::Default;
				stk.push(args);
				auto res_exp = expand_macro(macname, stk, map, buf);
				if (!res_exp.has_value()) {
					return res_exp;
				}
				buf.push_front(res_exp.value());
				args.clear();
			}
		}
		else {
			assert(0 && "Added extra parser state but did not change parser!");
		}
		ec = buf.peek_front(tok, state == ParserState::ParsingScope);
	}

	if (buf.size() != 0) {
		ErrType es;
		es.push_back("Found unparsable token");
		return std::unexpected(es);
	}
	return outbuf;
}
