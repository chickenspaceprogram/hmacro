#pragma once
#include <cu/string.h>

enum {
	HMACRO_TOK_MACRO,
	HMACRO_TOK_SCOPE,
	HMACRO_TOK_EXPANDER,
	HMACRO_TOK_QUOTER,
	HMACRO_TOK_TEXT,
	HMACRO_TOK_WS,

};
typedef struct {
	cu_string_view txt;
} hmacro_input_token;
