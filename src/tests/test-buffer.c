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

#include "../buffer.h"
#include <cu/dbgassert.h>

static void test_hm_buf(void)
{
	cu_string_view s1 = cu_cstr_cast("text from\nfirst file\n");
	cu_string_view s2 = cu_cstr_cast("text from\nsecond file");
	hm_buf buf;
	hm_buf_init(&buf);
	hm_buf_push(&buf, s1, NULL);
	hm_buf_push(&buf, s2, NULL);
	cu_string_view contents = hm_buf_contents(&buf);
	dbgassert(cu_streq(contents, cu_cstr_cast("text from\nsecond filetext from\nfirst file\n")));
	hm_buf_pop(&buf, 5);
	contents = hm_buf_contents(&buf);
	dbgassert(cu_streq(contents, cu_cstr_cast("from\nsecond filetext from\nfirst file\n")));

	hm_buf_free(&buf, NULL);
}

int main(void)
{
	test_hm_buf();
}
