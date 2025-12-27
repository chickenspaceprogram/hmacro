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
enum { // kind
	// Fundamental types
	HM_TYPE_FUNDAMENTAL,

	// Complex types
	HM_TYPE_SUM,
	HM_TYPE_PROD,
	HM_TYPE_KLEENE, // matches as many as possible of a child type
};

// fundamental and reserved typeIDs
enum {
	// Denotes a nameless type
	// Not a fundamental type, but reserved
	HM_RESERVED_NAMELESS = 0,

	// Fundamental typeIDs; these are syntactic concepts
	HM_ID_WS,
	HM_ID_ESCCHR,
	HM_ID_MACRO,
	HM_ID_BEGINTYPE,
	HM_ID_ALTERNATETYPE,
	HM_ID_ESCOPE,
	HM_ID_SCOPE,
	HM_ID_CHR,
	HM_ID_NUMERIC,

	// Reserved for the type of the def-args and the typedef-args
	HM_ID_DEF_ARGS,
	HM_ID_TYPEDEF_ARGS,

	HM_NUM_RESERVED_TYPEIDS,
};


typedef struct hm_type hm_type;
struct hm_type {
	size_t num_children;
	uintptr_t id;
	uint8_t kind;
	hm_type *children[];
};

typedef struct {
	
} hm_typelist;

// all of these assume txt has nonzero length
cu_str hm_parse_ws(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_escchr(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_macro(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_begintype(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_alternatetype(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_escope(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_scope(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_chr(const hm_tlit_lut *lut, cu_str *txt);
cu_str hm_parse_numeric(const hm_tlit_lut *lut, cu_str *txt);

hm_type *hm_type_create(size_t num_children, cu_arena *arena);


