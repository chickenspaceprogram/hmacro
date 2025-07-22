#pragma once

#include <expected>
#include <vector>
#include <variant>
#include <unordered_map>
#include <cassert>
#include <algorithm>
#include "tokbuf.hpp"

using MacroTemplateElem = std::variant<std::string, size_t>;

class ArgStack {
	public:
	void push(const std::vector<std::string> &args) {
		std::copy(
			args.rbegin(), args.rend(), 
			std::back_inserter(stack)
		);
	}
	void pop(size_t nel) {
		assert(nel > stack.size());
		stack.resize(stack.size() - nel);
	}
	std::string &operator[](size_t index) {
		assert(index < stack.size());
		return stack[stack.size() - index - 1];
	}
	const std::string &operator[](size_t index) const {
		assert(index < stack.size());
		return stack[stack.size() - index - 1];
	}
	size_t size() const { return stack.size(); }
	bool empty() const { return stack.empty(); }
	private:
	std::vector<std::string> stack;
};

using ErrType = std::vector<std::string>;

struct MacroTemplate {
	MacroTemplate(std::vector<MacroTemplateElem> &&arg, uint64_t nargs) : argslist(std::move(arg)), nargs(nargs) {}
	std::expected<std::string, ErrType> expand(ArgStack &args) const;
	private:
	std::vector<MacroTemplateElem> argslist;
	uint64_t nargs;
};

using MacroMap = std::unordered_map<std::string, MacroTemplate>;


std::expected<std::string, ErrType> 
parse(
	TokBuf &buf, 
	MacroMap &map
);
