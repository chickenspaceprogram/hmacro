#include "types.h"
#include <assert.h>
#include <cu/bitmanip.h>

#define DEFAULT_ID_CAPACITY (cu_bit_ceil(HM_NUM_FUND_TYPEIDS) << 2)

hm_parse_func hm_parse_fns[HM_NUM_FUND_TYPEIDS] = {
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

int hm_type_record_init(hm_type_record *rec, cu_arena *backing)
{
	rec->n_ids = 0;
	rec->idlist = NULL;
	rec->backing = backing;

	int retval = hm_type_record_reserve(rec, DEFAULT_ID_CAPACITY);
	if (retval != 0)
		return retval;
	rec->n_ids = HM_NUM_FUND_TYPEIDS;
	assert(rec->idlist != NULL && "should have an idlist");
	hm_type_block *cur_block = rec->idlist->next;
	assert(cur_block->next == NULL && "should only have one block");

	// fully initialized, rec is in a valid state from here on out
	
	cur_block->types[HM_ID_NULL] = NULL; // no fst elem

	size_t id = 1;

	for (; id < HM_NUM_FUND_TYPEIDS; ++id) {
		cur_block->types[id] = hm_type_alloc(rec, 0);
		if (cur_block->types[id] == NULL)
			return -1;
		cur_block->types[id]->id = id;
		cur_block->types[id]->kind = HM_KIND_FUNDAMENTAL;
		cur_block->types[id]->pf = hm_parse_fns[id];
	}
	return 0;
}

int hm_type_record_reserve(hm_type_record *rec, size_t new_n_ids)
{
	if (rec->idlist == NULL) {
		size_t blocksz = cu_bit_ceil(new_n_ids) * sizeof(hm_type *)
			+ sizeof(hm_type_block);
		rec->idlist = cu_arena_alloc(blocksz, rec->backing);
		return rec->idlist == NULL ? -1 : 0;
	}
	size_t nel = 0;
	hm_type_block *last = NULL;
	for (hm_type_block *blk = rec->idlist; blk != NULL; blk = blk->next) {
		nel += blk->capacity;
		if (nel >= new_n_ids)
			return 0;
		last = blk;
	}
	assert(last != NULL && "should have a last ptr");

	size_t new_block_sz = cu_bit_ceil(nel + new_n_ids) - nel;
	new_block_sz =
		new_block_sz * sizeof(hm_type *) + sizeof(hm_type_block);
	last->next = cu_arena_alloc(new_block_sz, rec->backing);
	return last->next == NULL ? -1 : 0;
}

static inline hm_type **rec_lookup_internal(hm_type_record *rec, size_t id)
{
	if (id < rec->n_ids)
		return NULL;
	for (hm_type_block *blk = rec->idlist; blk != NULL; blk = blk->next) {
		if (id < blk->capacity)
			return blk->types + id;
		id -= blk->capacity;
	}
	assert(0 && "record invariants violated");
	return NULL;
}
size_t hm_type_record_register(hm_type_record *rec, hm_type *type)
{
	int rv = hm_type_record_reserve(rec, rec->n_ids + 1);
	if (rv != 0)
		return rv;
	++rec->n_ids;
	hm_type **res = rec_lookup_internal(rec, rec->n_ids - 1);
	assert(res != NULL && "should have space for the type");
	*res = type;
	return rec->n_ids - 1;
}
hm_type *hm_type_record_lookup(hm_type_record *rec, size_t id)
{
	hm_type **res = rec_lookup_internal(rec, id);
	if (res == NULL)
		return NULL;
	return *res;
}
