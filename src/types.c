#include "types.h"
#include <assert.h>

#define ERROR_STR (cu_str){ .buf = NULL, .len = 0 }

cu_str hm_parse_fund_type(uintptr_t *id, const hm_tlit_lut *lut, cu_str txt)
{
	if (txt.len == 0)
		return ERROR_STR;
	uint8_t fst_ctype = hm_tlit_octet(lut, txt.buf[0]);
	bool is_basic = fst_ctype == HM_TEXT
			|| fst_ctype == HM_END_QUOTE
			|| fst_ctype == HM_END_EXPAND
			|| fst_ctype == HM_MACRO_NAME;
	if (is_basic) {
		*id = HM_FUND_CHR;
		return (cu_str){
			.buf = txt.buf,
			.len = 1,
		};
	}
	if (fst_ctype == HM_WHITESPACE) {
		*id = HM_FUND_WS;
		for (size_t i = 1; i < txt.len; ++i) {
			if (hm_tlit_octet(lut, txt.buf[i]) != HM_WHITESPACE) {
				return (cu_str){
					.buf = txt.buf,
					.len = i,
				};
			}
		}
		return txt;
	}
	if (fst_ctype == HM_MACRO_SIGN || fst_ctype == HM_TYPESIGN || fst_ctype == HM_TYPEALTERNATE) {
		size_t fst_namechr = 1;
		while (hm_tlit_octet(lut, txt.buf[fst_namechr]) == HM_WHITESPACE) {
			++fst_namechr;
		}
		if (hm_tlit_octet(lut, txt.buf[fst_namechr]) != HM_MACRO_NAME) {
			*id = HM_FUND_CHR;
			return (cu_str){
				.buf = txt.buf,
				.len = 1,
			};
		}
		switch (fst_ctype) {
		case HM_MACRO_SIGN:
			*id = HM_FUND_MACRO;
			break;
		case HM_TYPESIGN:
			*id = HM_FUND_BEGINTYPE;
			break;
		case HM_TYPEALTERNATE:
			*id = HM_FUND_TYPEALTERNATE;
			break;
		default:
			assert(0 && "Error in configuring if/switch statement, contact the developers");
			break;
		}
		for (size_t i = fst_namechr + 1; i < txt.len; ++i) {
			uint8_t ctype = hm_tlit_octet(lut, txt.buf[i]);
			if (ctype != HM_MACRO_NAME && ctype != HM_NUMERIC)
				return (cu_str){
					.buf = txt.buf + fst_namechr,
					.len = i,
				};
		}
	}
	if (fst_ctype == HM_BEGIN_QUOTE) {
		*id = HM_FUND_QSCOPE;
		size_t nbrack = 1;
		for (size_t i = 1; i < txt.len; ++i) {
			uint8_t ctype = hm_tlit_octet(lut, txt.buf[i]);
			if (ctype == HM_BEGIN_QUOTE)
				++nbrack;
			else if (ctype == HM_END_QUOTE)
				--nbrack;
			if (nbrack == 0)
				return (cu_str){
					.buf = txt.buf + 1,
					.len = i - 1,
				};
		}
		*id = HM_FUND_CHR;
		return (cu_str){
			.buf = txt.buf,
			.len = 1,
		};
	}
	if (fst_ctype == HM_BEGIN_EXPAND) {
		*id = HM_FUND_ESCOPE;
		size_t nbrack = 1;
		for (size_t i = 1; i < txt.len; ++i) {
			uint8_t ctype = hm_tlit_octet(lut, txt.buf[i]);
			if (ctype == HM_BEGIN_EXPAND)
				++nbrack;
			else if (ctype == HM_END_EXPAND)
				--nbrack;
			if (nbrack == 0)
				return (cu_str){
					.buf = txt.buf,
					.len = i + 1,
				};
		}
		*id = HM_FUND_CHR;
		return (cu_str){
			.buf = txt.buf,
			.len = 1,
		};
	}
	if (fst_ctype == HM_NUMERIC) {
		*id = HM_FUND_NUMERIC;
		for (size_t i = 1; i < txt.len; ++i) {
			uint8_t ctype = hm_tlit_octet(lut, txt.buf[i]);
			if (ctype != HM_NUMERIC)
				return (cu_str){
					.buf = txt.buf,
					.len = i,
				};
		}
		return txt;
	}
	if (fst_ctype == HM_NEGATIVE) {
		if (hm_tlit_octet(lut, txt.buf[1]) != HM_NUMERIC) {
			*id = HM_FUND_CHR;
			return (cu_str){
				.buf = txt.buf,
				.len = 1,
			};
		}
		*id = HM_FUND_NUMERIC;
		for (size_t i = 2; i < txt.len; ++i) {
			uint8_t ctype = hm_tlit_octet(lut, txt.buf[i]);
			if (ctype != HM_NUMERIC)
				return (cu_str){
					.buf = txt.buf,
					.len = i,
				};
		}
		return txt;
	}
	assert(0 && "failed to parse!");
	return ERROR_STR;
}
