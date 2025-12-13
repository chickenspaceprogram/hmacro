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

#include "tlit.h"
#include <string.h>

void hm_tlit_set_default(hm_tlit_lut *lut)
{
	memset(lut->lut, HM_TEXT, 0x100);
	for (size_t i = '0'; i <= '9'; ++i) {
		lut->lut[i] = i - '0';
	}
	lut->lut['$'] = HM_EXPANDER;
	lut->lut['#'] = HM_QUOTER;
	lut->lut['\\'] = HM_MACRO_START;
	lut->lut['{'] = HM_BEGIN_SCOPE;
	lut->lut['}'] = HM_END_SCOPE;

	lut->lut['-'] = HM_MACRO_NAME;
	lut->lut['_'] = HM_MACRO_NAME;
	for (size_t i = 'a'; i <= 'z'; ++i) {
		lut->lut[i] = HM_MACRO_NAME;
	}
	for (size_t i = 'A'; i <= 'Z'; ++i) {
		lut->lut[i] = HM_MACRO_NAME;
	}

	lut->lut['\t'] = HM_WHITESPACE;
	lut->lut['\n'] = HM_WHITESPACE;
	lut->lut['\v'] = HM_WHITESPACE;
	lut->lut['\f'] = HM_WHITESPACE;
	lut->lut['\r'] = HM_WHITESPACE;
	lut->lut[' '] = HM_WHITESPACE;
}

