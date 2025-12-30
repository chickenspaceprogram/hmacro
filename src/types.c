// hmacro - a macro preprocessor
// Copyright (C) 2025 Athena Boose

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

// SPDX-License-Identifier: GPL-3.0-or-later

#include "types.h"
#include <assert.h>
#include <cu/bitmanip.h>

#define DEFAULT_ID_CAPACITY (cu_bit_ceil(HM_NUM_FUND_TYPEIDS) << 2)
#define DEFAULT_AST_VL_CAPACITY 16

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
static inline cu_str
hm_parse_ident(const hm_tlit_lut *lut, cu_str *txt, uint8_t fst_ctype)
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
	if (id >= rec->n_ids)
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
	int rv = hm_type_record_reserve(rec, ++rec->n_ids);
	if (rv != 0)
		return rv;
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


typedef struct hm_ast_veclist_block hm_ast_veclist_block;
static inline hm_ast_veclist hm_ast_veclist_init(void)
{
	return (hm_ast_veclist){
		.nel = 0,
		.elems = NULL,
	};
}

struct hm_ast_veclist_block {
	hm_ast_veclist_block *next;
	size_t capacity;
	hm_ast_node *data[];
};


static inline hm_ast_node **ast_veclist_find_internal(hm_ast_veclist *vl,
	size_t ind)
{
	if (ind >= vl->nel)
		return NULL;
	for (hm_ast_veclist_block *blk = vl->elems; blk != NULL;
		blk = blk->next
	) {
		if (ind < blk->capacity) {
			return blk->data + ind;
		}
		ind -= blk->capacity;
	}
	assert(0 && "veclist invariants violated");
}

static inline int hm_ast_veclist_reserve(hm_ast_veclist *vl, size_t nel,
	cu_arena *arena)
{
	if (nel == 0)
		return 0;
	if (vl->elems == NULL) {
		size_t minsz = cu_bit_ceil(nel);
		minsz = minsz < DEFAULT_AST_VL_CAPACITY 
			? DEFAULT_AST_VL_CAPACITY : minsz;
		vl->elems = cu_arena_alloc(minsz * sizeof(hm_ast_node *)
			+ sizeof(hm_ast_veclist_block), arena);
		if (vl->elems == NULL)
			return -1;
		else
			return 0;
	}

	size_t total_sz = 0;
	hm_ast_veclist_block *last_blk;
	for (hm_ast_veclist_block *blk = vl->elems; blk != NULL;
		blk = blk->next
	) {
		total_sz += blk->capacity;
		last_blk = blk;
	}
	assert(last_blk != NULL && "shouldn't have last elem be null");
	size_t alloc_sz = cu_bit_ceil(total_sz + nel) - total_sz;
	alloc_sz = alloc_sz * sizeof(hm_ast_node *)
		+ sizeof(hm_ast_veclist_block);
	last_blk->next = cu_arena_alloc(alloc_sz, arena);
	if (last_blk->next == NULL)
		return -1;
	return 0;
}
static inline int hm_ast_veclist_push(hm_ast_veclist *vl, hm_ast_node *nd,
	cu_arena *arena)
{
	int retval = hm_ast_veclist_reserve(vl, ++vl->nel, arena);
	if (retval != 0)
		return retval;
	hm_ast_node **nd_ptr = ast_veclist_find_internal(vl, vl->nel - 1);
	assert(nd != NULL && "should have space for nd");
	*nd_ptr = nd;
	return 0;
}
hm_ast_node *hm_ast_veclist_at(hm_ast_veclist *vl, size_t ind)
{
	hm_ast_node **nd = ast_veclist_find_internal(vl, ind);
	if (nd == NULL)
		return NULL;
	return *nd;
}




hm_ast_node *
hm_ast_generate(cu_str *txt, hm_type *target, const hm_tlit_lut *lut,
	cu_arena *backing)
{
	if (txt->len == 0)
		return NULL;
	hm_ast_node *nd = NULL;
	if (target->kind == HM_KIND_FUNDAMENTAL) {
		cu_str item = target->pf(lut, txt);
		if (cu_str_isnil(item))
			return NULL;
		nd = cu_arena_alloc(sizeof(hm_ast_node), backing);
		nd->id = target->id;
		nd->txt = item;
	}
	else if (target->kind == HM_KIND_SUM) {
		for (size_t i = 0; i < target->num_children; ++i) {
			cu_str tmp = *txt;
			nd = hm_ast_generate(&tmp, target->children[i],
				lut, backing);
			if (nd != NULL) {
				*txt = tmp;
				break;
			}
			nd = NULL;
		}
	}
	else if (target->kind == HM_KIND_PROD) {
		{
			size_t bufsz = sizeof(hm_ast_node) 
				+ sizeof(hm_ast_node *) * target->num_children;
			nd = cu_arena_alloc(bufsz, backing);
		}
		cu_str tmp = *txt;
		nd->id = target->id;
		for (size_t i = 0; i < target->num_children; ++i) {
			nd->children[i] = hm_ast_generate(&tmp,
				target->children[i], lut, backing);
			if (nd->children[i] == NULL)
				return NULL;
		}
		*txt = tmp;
	}
	else if (target->kind == HM_KIND_KLEENE) {
		hm_ast_veclist vl = hm_ast_veclist_init();
		cu_str tmp = *txt;
		for (
			nd = hm_ast_generate(
				&tmp, target->kleene_type, lut, backing);
			nd != NULL;
			nd = hm_ast_generate(
				&tmp, target->kleene_type, lut, backing)
		) {
			hm_ast_veclist_push(&vl, nd, backing);
		}
		nd = cu_arena_alloc(sizeof(hm_ast_node), backing);
		if (nd == NULL)
			return NULL;
		nd->id = target->id;
		nd->kleene_elems = vl;
		*txt = tmp;
	}
	else {
		assert(0 && "Invariants violated");
	}
	return nd;

}
