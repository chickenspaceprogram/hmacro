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
	HM_TEXT,
	HM_NUMERIC, // not user-changeable
	HM_NEGATIVE,		// `-`
	HM_BEGIN_QUOTE,		// `{`
	HM_END_QUOTE,		// `}`
	HM_BEGIN_EXPAND,	// `[`
	HM_END_EXPAND,		// `]`
	HM_MACRO_SIGN,		// `\`
	HM_TYPESIGN,		// `!`
	HM_TYPEALTERNATE,	// `:`
	HM_MACRO_NAME,		// [a-zA-Z_]
	HM_WHITESPACE,		// [ \t\n\v\f\r]
} hm_chartypes;

void hm_tlit_set_default(hm_tlit_lut *lut);

