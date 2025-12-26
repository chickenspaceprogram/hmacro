#include "types.h"
#include <assert.h>

#define NIL_STR (cu_str){ .buf = NULL, .len = 0 }

cu_str hm_parse_fund_type(uintptr_t *id, const hm_tlit_lut *lut, cu_str *txt)
{
	if (txt->len == 0)
		return NIL_STR;
	uint8_t fst_ctype = hm_tlit_octet(lut, txt->buf[0]);
	bool is_basic = fst_ctype == HM_TEXT
			|| fst_ctype == HM_END_QUOTE
			|| fst_ctype == HM_MACRO_NAME;
	if (is_basic) {
		*id = HM_FUND_CHR;
		cu_str retval = {
			.buf = txt->buf,
			.len = 1,
		};
		++(txt->buf);
		--(txt->len);
		return retval;
	}
	if (fst_ctype == HM_WHITESPACE) {
		*id = HM_FUND_WS;
		for (size_t i = 1; i < txt->len; ++i) {
			if (hm_tlit_octet(lut, txt->buf[i]) != HM_WHITESPACE) {
				cu_str retval = {
					.buf = txt->buf,
					.len = i,
				};
				txt->buf += i;
				txt->len -= i;
				return retval;
			}
		}
		cu_str retval = *txt;
		txt->buf += txt->len;
		txt->len = 0;
		return retval;
	}
	if (fst_ctype == HM_MACRO_SIGN) {
		if (txt->len == 1) {
			*id = HM_FUND_CHR;
			cu_str retval = *txt;
			++(txt->buf);
			txt->len = 0;
			return retval;
		}
		uint8_t snd_ctype = hm_tlit_octet(lut, txt->buf[2]);
		if (snd_ctype == HM_WHITESPACE) {
			txt->buf += 2;
			txt->len -= 2;
			return NIL_STR;
		}
		if (snd_ctype != HM_MACRO_NAME) {
			// must be escaped!
			*id = HM_FUND_ESCCHR;
			cu_str retval = {
				.buf = txt->buf + 1,
				.len = 1
			};
			txt->buf += 2;
			txt->len -= 2;
			return retval;
		}
		*id = HM_FUND_MACRO;
		for (size_t i = 1; i < txt->len; ++i) {
			uint8_t ctype = hm_tlit_octet(lut, txt->buf[i]);
			if (ctype != HM_MACRO_NAME && ctype != HM_NUMERIC) {
				cu_str retval = {
					.buf = txt->buf + 1,
					.len = i,
				};
				txt->buf += i;
				txt->len -= i;
				return retval;
			}
		}
		cu_str retval = *txt;
		txt->buf += txt->len;
		txt->len = 0;
		return retval;
	}
	if (fst_ctype == HM_TYPESIGN || fst_ctype == HM_TYPEALTERNATE) {
		if (hm_tlit_octet(lut, txt->buf[1]) != HM_MACRO_NAME) {
			*id = HM_FUND_CHR;
			cu_str retval = {
				.buf = txt->buf,
				.len = 1,
			};
			++(txt->buf);
			--(txt->len);
			return retval;
		}
		switch (fst_ctype) {
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
		for (size_t i = 1; i < txt->len; ++i) {
			uint8_t ctype = hm_tlit_octet(lut, txt->buf[i]);
			if (ctype != HM_MACRO_NAME && ctype != HM_NUMERIC) {
				cu_str retval = {
					.buf = txt->buf + 1,
					.len = i,
				};
				txt->buf += i;
				txt->len -= i;
				return retval;
			}
		}
		cu_str retval = *txt;
		txt->buf += txt->len;
		txt->len = 0;
		return retval;
	}
	if (fst_ctype == HM_BEGIN_QUOTE) {
		*id = HM_FUND_QSCOPE;
		size_t nbrack = 1;
		for (size_t i = 1; i < txt->len; ++i) {
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

hm_type *hm_type_create(size_t num_children, cu_arena *arena)
{
	// mult could overflow, but it's probably fine
	hm_type *buf = cu_arena_alloc(sizeof(hm_type) + sizeof(hm_type *) * num_children, arena);
	if (buf == NULL)
		return NULL;
	buf->num_children = num_children;
	buf->id = 0;
	buf->kind = 0;
	memset(buf->children, 0, sizeof(hm_type *) * num_children);
	return buf;
}
