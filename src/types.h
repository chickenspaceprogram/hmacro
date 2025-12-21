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


// Kinds of types
enum { // kind
	// Fundamental types
	HM_TYPE_FUNDAMENTAL,

	// Complex types
	HM_TYPE_SUM,
	HM_TYPE_PROD,
	HM_TYPE_KLEENE, // matches as many as possible of a child type
};

// fundamental typeIDs
enum {
	// Denotes a nameless type
	HM_TYPE_NAMELESS = 0,

	HM_FUND_CHR,
	HM_FUND_WS,
	HM_FUND_MACRO,
	HM_FUND_TYPE,
	HM_FUND_QSCOPE,
	HM_FUND_ESCOPE,
};


typedef struct hm_type hm_type;
struct hm_type {
	size_t num_children;
	uintptr_t id;
	uint8_t kind;
	hm_type *child_ids[];
};

