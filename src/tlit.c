#include "tlit.h"
#include <string.h>

void hm_tlit_set_default(hm_tlit_lut *lut)
{
	memset(lut->lut, HM_TEXT, 0x100);
	for (size_t i = '0'; i <= '9'; ++i) {
		lut->lut[i] = i - '0';
	}
	lut->lut['$'] = HM_EXPANDER;
	lut->lut['#'] = HM_QUOTER;
	lut->lut['\\'] = HM_MACRO_START;
	lut->lut['{'] = HM_BEGIN_SCOPE;
	lut->lut['}'] = HM_END_SCOPE;

	lut->lut['-'] = HM_MACRO_NAME;
	lut->lut['_'] = HM_MACRO_NAME;
	for (size_t i = 'a'; i <= 'z'; ++i) {
		lut->lut[i] = HM_MACRO_NAME;
	}
	for (size_t i = 'A'; i <= 'Z'; ++i) {
		lut->lut[i] = HM_MACRO_NAME;
	}

	lut->lut['\t'] = HM_WHITESPACE;
	lut->lut['\n'] = HM_WHITESPACE;
	lut->lut['\v'] = HM_WHITESPACE;
	lut->lut['\f'] = HM_WHITESPACE;
	lut->lut['\r'] = HM_WHITESPACE;
	lut->lut[' '] = HM_WHITESPACE;
}

