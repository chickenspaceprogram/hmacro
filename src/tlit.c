#include "tlit.h"
#include <string.h>

void hmacro_tlit_set_default(hmacro_tlit_lut *lut)
{
	memset(lut->lut, HMACRO_TEXT, 0x100);
	for (size_t i = '0'; i <= '9'; ++i) {
		lut->lut[i] = i - '0';
	}
	lut->lut['$'] = HMACRO_EXPANDER;
	lut->lut['#'] = HMACRO_QUOTER;
	lut->lut['\\'] = HMACRO_MACRO_START;
	lut->lut['{'] = HMACRO_BEGIN_SCOPE;
	lut->lut['}'] = HMACRO_END_SCOPE;

	lut->lut['-'] = HMACRO_MACRO_NAME;
	lut->lut['_'] = HMACRO_MACRO_NAME;
	for (size_t i = 'a'; i <= 'z'; ++i) {
		lut->lut[i] = HMACRO_MACRO_NAME;
	}
	for (size_t i = 'A'; i <= 'Z'; ++i) {
		lut->lut[i] = HMACRO_MACRO_NAME;
	}

	lut->lut['\t'] = HMACRO_WHITESPACE;
	lut->lut['\n'] = HMACRO_WHITESPACE;
	lut->lut['\v'] = HMACRO_WHITESPACE;
	lut->lut['\f'] = HMACRO_WHITESPACE;
	lut->lut['\r'] = HMACRO_WHITESPACE;
	lut->lut[' '] = HMACRO_WHITESPACE;
}

