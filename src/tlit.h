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
#include <stddef.h>

// hm TransLITerator
//
// (i suck at acronyms)
//
// this takes a stream of octets and converts it into a stream of different octets

typedef struct {
	uint8_t lut[0x100];
} hm_tlit_lut;

static inline uint8_t
hm_tlit_octet(
	const hm_tlit_lut *lut,
	uint8_t chr
) {
	return lut->lut[chr];
}

static inline void
hm_tlit_stream(
	const hm_tlit_lut *lut,
	uint8_t *outbuf,
	const uint8_t *inbuf,
	size_t bufsz
) {
	for (size_t i = 0; i < bufsz; ++i) {
		outbuf[i] = lut->lut[inbuf[i]];
	}
}

typedef enum {
	HM_NUMERIC_0,
	HM_NUMERIC_1,
	HM_NUMERIC_2,
	HM_NUMERIC_3,
	HM_NUMERIC_4,
	HM_NUMERIC_5,
	HM_NUMERIC_6,
	HM_NUMERIC_7,
	HM_NUMERIC_8,
	HM_NUMERIC_9,
	HM_TEXT,
	HM_EXPANDER,
	HM_QUOTER,
	HM_MACRO_START,
	HM_BEGIN_SCOPE,
	HM_END_SCOPE,
	HM_MACRO_NAME,
	HM_WHITESPACE,
} hm_chartypes;

void hm_tlit_set_default(hm_tlit_lut *lut);

