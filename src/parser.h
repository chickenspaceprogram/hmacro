#pragma once
#include <cu/string.h>

enum {
	HM_TOK_MACRO,
	HM_TOK_SCOPE,
	HM_TOK_EXPANDER,
	HM_TOK_QUOTER,
	HM_TOK_TEXT,
	HM_TOK_WS,

};
typedef struct {
	cu_string_view txt;
} hm_input_token;
