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

#pragma once

#include <stdint.h>
#include <cu/string.h>
#include <cu/arena.h>
#include "tlit.h"


// Kinds of types
enum {
	// Type without a kind; placeholder
	HM_KIND_NULL = 0,

	// Fundamental type; parsed with a provided function pointer
	HM_KIND_FUNDAMENTAL,

	// Algebraic types
	HM_KIND_SUM, // parsed as any of its children
	HM_KIND_PROD, // parsed as all of its children, in sequence

	// Kleene type; matches an infinite sequence of a given type
	HM_KIND_KLEENE,
};

typedef cu_str (*hm_parse_func)(const hm_tlit_lut *lut, cu_str *txt);

typedef struct hm_type hm_type;
struct hm_type {
	size_t id;
	uint8_t kind;
	union {
		struct {
			size_t num_children;
			hm_type *children[];
		};
		hm_type *kleene_type;
		hm_parse_func pf;
	};
};

typedef struct hm_type_block hm_type_block;
struct hm_type_block {
	hm_type_block *next;
	size_t capacity;
	hm_type *types[];
};


typedef struct {
	size_t n_ids;
	hm_type_block *idlist;
	cu_arena *backing;
} hm_type_record;

// Initializes the hm_type_record with the fundamental types.
int hm_type_record_init(hm_type_record *rec, cu_arena *backing);
int hm_type_record_reserve(hm_type_record *rec, size_t new_n_ids);

// Registers a new type to the record, returns its id
//
// If this fails, 0 is returned.
// IDs are therefore nonzero.
size_t hm_type_record_register(hm_type_record *rec, hm_type *type);

hm_type *hm_type_record_lookup(hm_type_record *rec, size_t id);

static inline hm_type *hm_type_alloc(hm_type_record *rec, size_t nchild)
{
	return cu_arena_alloc(sizeof(hm_type) + nchild * sizeof(hm_type *), rec->backing);
}


// fundamental and reserved typeIDs
enum {
	// Denotes a nameless, placeholder type
	HM_ID_NULL = 0,

	// Fundamental typeIDs; these are syntactic concepts
	HM_ID_WS,
	HM_ID_ESCCHR,
	HM_ID_KLEENE,
	HM_ID_EXPANDER,
	HM_ID_NAMESPACE,
	HM_ID_MACRO,
	HM_ID_BEGINTYPE,
	HM_ID_ALTERNATETYPE,
	HM_ID_SCOPE,
	HM_ID_CHR,
	HM_ID_NUMERIC,

	HM_NUM_FUND_TYPEIDS,
};

extern hm_parse_func hm_parse_fns[HM_NUM_FUND_TYPEIDS];

// all of these assume txt has nonzero length
cu_str hm_parse_ws(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_escchr(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_kleene(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_expander(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_namespace(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_macro(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_begintype(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_alternatetype(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_scope(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_chr(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_numeric(const hm_tlit_lut *lut, cu_str *txt);


typedef struct hm_ast_node hm_ast_node;
typedef struct hm_ast_veclist_block hm_ast_veclist_block;
typedef struct {
	size_t nel;
	hm_ast_veclist_block *elems;
} hm_ast_veclist;

hm_ast_node *hm_ast_veclist_at(hm_ast_veclist *vl, size_t ind);
static inline size_t hm_ast_veclist_len(const hm_ast_veclist *vl)
{
	return vl->nel;
}

struct hm_ast_node {
	size_t id;
	union {
		struct {
			size_t num_children;
			hm_ast_node *children[];
		};
		hm_ast_veclist kleene_elems;
		cu_str txt;
	};
};

hm_ast_node *
hm_ast_generate(cu_str *txt, hm_type *target, const hm_tlit_lut *lut,
	cu_arena *backing);
