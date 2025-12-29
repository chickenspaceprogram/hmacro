#include "types.h"
#include <assert.h>
#include <cu/bitmanip.h>

#define DEFAULT_TREEBUF_SZ 0x10000
extern hm_parse_func hm_parse_fns[HM_NUM_FUND_TYPEIDS] = {
	NULL,
	hm_parse_ws,
	hm_parse_escchr,
	hm_parse_kleene,
	hm_parse_expander,
	hm_parse_namespace,
	hm_parse_macro,
	hm_parse_begintype,
	hm_parse_alternatetype,
	hm_parse_scope,
	hm_parse_chr,
	hm_parse_numeric,
};


cu_str hm_parse_ws(const hm_tlit_lut *lut, cu_str *txt)
{
	assert(txt->len > 0 && "string must have nonzero length to be parsed");
	size_t nws = 0;
	for (nws = 0; nws < txt->len; ++nws) {
		if (hm_tlit_octet(lut, txt->buf[nws]) != HM_WHITESPACE) {
			break;
		}
	}
	if (nws == 0)
		return CU_NIL_STR;
	cu_str retval = cu_str_substr(*txt, 0, nws);
	*txt = cu_str_rmprefix(*txt, nws);
	return retval;
}
cu_str hm_parse_escchr(const hm_tlit_lut *lut, cu_str *txt)
{
	assert(txt->len > 0 && "string must have nonzero length to be parsed");
	if (txt->len < 2)
		return CU_NIL_STR;
	if (hm_tlit_octet(lut, txt->buf[0]) != HM_MACRO_SIGN)
		return CU_NIL_STR;
	if (hm_tlit_octet(lut, txt->buf[1]) == HM_MACRO_NAME)
		return CU_NIL_STR;
	cu_str retval = cu_str_substr(*txt, 1, 2);
	*txt = cu_str_rmprefix(*txt, 2);
	return retval;
}
cu_str hm_parse_kleene(const hm_tlit_lut *lut, cu_str *txt)
{
	assert(txt->len > 0 && "string must have nonzero length to be parsed");
	if (hm_tlit_octet(lut, txt->buf[0]) != HM_KLEENE)
		return CU_NIL_STR;
	cu_str retval = cu_str_substr(*txt, 0, 1);
	*txt = cu_str_rmprefix(*txt, 1);
	return retval;
}
cu_str hm_parse_expander(const hm_tlit_lut *lut, cu_str *txt)
{
	assert(txt->len > 0 && "string must have nonzero length to be parsed");
	if (hm_tlit_octet(lut, txt->buf[0]) != HM_EXPANDER)
		return CU_NIL_STR;
	cu_str retval = cu_str_substr(*txt, 0, 1);
	*txt = cu_str_rmprefix(*txt, 1);
	return retval;
}
cu_str hm_parse_namespace(const hm_tlit_lut *lut, cu_str *txt)
{
	assert(txt->len > 0 && "string must have nonzero length to be parsed");
	if (txt->len < 2)
		return CU_NIL_STR;
	if (hm_tlit_octet(lut, txt->buf[0]) != HM_TYPEALTERNATE)
		return CU_NIL_STR;
	if (hm_tlit_octet(lut, txt->buf[1]) != HM_TYPEALTERNATE)
		return CU_NIL_STR;
	cu_str retval = cu_str_substr(*txt, 0, 2);
	*txt = cu_str_rmprefix(*txt, 1);
	return retval;
}
static inline cu_str hm_parse_ident(const hm_tlit_lut *lut, cu_str *txt, uint8_t fst_ctype)
{
	assert(txt->len > 0 && "string must have nonzero length to be parsed");
	if (txt->len < 2)
		return CU_NIL_STR;
	if (hm_tlit_octet(lut, txt->buf[0]) != fst_ctype)
		return CU_NIL_STR;
	if (hm_tlit_octet(lut, txt->buf[1]) != HM_MACRO_NAME)
		return CU_NIL_STR;
	size_t end_loc;
	for (end_loc = 2; end_loc < txt->len; ++end_loc) {
		if (hm_tlit_octet(lut, txt->buf[end_loc]) != HM_MACRO_NAME)
			break;
	}
	cu_str retval = cu_str_substr(*txt, 1, end_loc);
	*txt = cu_str_rmprefix(*txt, end_loc);
	return retval;
}

cu_str hm_parse_macro(const hm_tlit_lut *lut, cu_str *txt)
{
	return hm_parse_ident(lut, txt, HM_MACRO_SIGN);
}
cu_str hm_parse_begintype(const hm_tlit_lut *lut, cu_str *txt)
{
	return hm_parse_ident(lut, txt, HM_TYPESIGN);
}
cu_str hm_parse_alternatetype(const hm_tlit_lut *lut, cu_str *txt)
{
	return hm_parse_ident(lut, txt, HM_TYPEALTERNATE);
}
cu_str hm_parse_scope(const hm_tlit_lut *lut, cu_str *txt)
{
	assert(txt->len > 0 && "string must have nonzero length to be parsed");
	if (txt->len < 2)
		return CU_NIL_STR;
	if (hm_tlit_octet(lut, txt->buf[0]) != HM_BEGIN_SCOPE)
		return CU_NIL_STR;
	size_t end_chr = 1;
	size_t bracket_ct = 1;
	for (; end_chr < txt->len; ++end_chr) {
		uint8_t ctype = hm_tlit_octet(lut, txt->buf[end_chr]);
		if (ctype == HM_BEGIN_SCOPE)
			++bracket_ct;
		else if (ctype == HM_END_SCOPE)
			--bracket_ct;
		if (bracket_ct == 0)
			break;
	}
	if (bracket_ct != 0)
		return CU_NIL_STR;
	cu_str retval = cu_str_substr(*txt, 1, end_chr);
	*txt = cu_str_rmprefix(*txt, end_chr + 1);
	return retval;
}
cu_str hm_parse_chr(const hm_tlit_lut *lut, cu_str *txt)
{
	assert(txt->len > 0 && "string must have nonzero length to be parsed");
	cu_str retval = cu_str_substr(*txt, 0, 1);
	*txt = cu_str_rmprefix(*txt, 1);
	return retval;
}
cu_str hm_parse_numeric(const hm_tlit_lut *lut, cu_str *txt)
{
	assert(txt->len > 0 && "string must have nonzero length to be parsed");
	uint8_t fst_ctype = hm_tlit_octet(lut, txt->buf[0]);
	if (fst_ctype != HM_NUMERIC && fst_ctype != HM_NEGATIVE)
		return CU_NIL_STR;
	size_t start_loc = 0;
	size_t end_loc = 1;
	if (fst_ctype == HM_NEGATIVE) {
		if (hm_tlit_octet(lut, txt->buf[1]) != HM_NUMERIC)
			return CU_NIL_STR;
		++end_loc;
		++start_loc;
	}
	for (; end_loc < txt->len; ++end_loc) {
		if (hm_tlit_octet(lut, txt->buf[end_loc]) != HM_NUMERIC)
			break;
	}
	cu_str retval = cu_str_substr(*txt, start_loc, end_loc);
	*txt = cu_str_rmprefix(*txt, end_loc);
	return retval;
}

int hm_type_record_init(hm_type_record *rec, cu_alloc *alloc)
{
	rec->n_ids = 0;
	rec->capacity = 0;
	rec->fst_id = NULL;
	rec->alloc = alloc;

	rec->elem_backing = cu_arena_new(DEFAULT_TREEBUF_SZ, alloc);
	if (rec->elem_backing == NULL)
		return -1;
	int retval = hm_type_record_reserve(rec, HM_NUM_FUND_TYPEIDS);
	if (retval != 0) {
		cu_arena_free(rec->elem_backing);
		return retval;
	}

	rec->n_ids = HM_NUM_FUND_TYPEIDS;

	// fully initialized, rec is in a valid state from here on out
	
	rec->fst_id[HM_ID_NULL] = NULL; // no fst elem

	size_t id = 1;

	for (; id < HM_NUM_FUND_TYPEIDS; ++id) {
		rec->fst_id[id] = hm_type_alloc(rec, 0);
		if (rec->fst_id[id] == NULL) {
			// error, cleanup
			hm_type_record_free(rec);
			return -1;
		}
		rec->fst_id[id]->id = id;
		rec->fst_id[id]->kind = HM_KIND_FUNDAMENTAL;
		rec->fst_id[id]->pf = hm_parse_fns[id];
	}
	return 0;
}

int hm_type_record_reserve(hm_type_record *rec, size_t new_n_ids)
{
	if (new_n_ids <= rec->capacity)
		return 0;
	
	size_t new_sz = cu_bit_ceil(new_n_ids);
	if (rec->fst_id == NULL) {
		rec->fst_id = cu_allocarray(new_sz, sizeof(hm_type *), rec->alloc);
		if (rec->fst_id == NULL)
			return -1;
	}
	else {
		int retval = cu_try_reallocarray((void **)&rec->fst_id, new_sz, rec->capacity, sizeof(hm_type *), rec->alloc);
		if (retval != 0)
			return retval;
	}
	rec->capacity = new_sz;
	return 0;
}
