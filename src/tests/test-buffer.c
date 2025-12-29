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
	cu_str s1 = cu_str_from_cstr("text from\nfirst file\n");
	cu_str s2 = cu_str_from_cstr("text from\nsecond file");
	hm_buf buf;
	hm_buf_init(&buf);
	hm_buf_push(&buf, s1, NULL);
	hm_buf_push(&buf, s2, NULL);
	cu_str contents = hm_buf_contents(&buf);
	dbgassert(cu_str_eq(contents, cu_str_from_cstr("text from\nsecond filetext from\nfirst file\n")));
	hm_buf_pop(&buf, 5);
	contents = hm_buf_contents(&buf);
	dbgassert(cu_str_eq(contents, cu_str_from_cstr("from\nsecond filetext from\nfirst file\n")));

	hm_buf_free(&buf, NULL);
}

static void test_hm_tag(void)
{
	cu_str s1 = cu_str_from_cstr("text from\nfirst file\n");
	cu_str s2 = cu_str_from_cstr("text from\nsecond file");
	hm_tag tag1 = HM_TAG_DEFAULT(cu_str_from_cstr("tag one"), s1.len);
	hm_tag tag2 = HM_TAG_DEFAULT(cu_str_from_cstr("tag two"), s2.len);
	// pretending s1 pushed onto buf, then s2
	hm_taglist tl;
	hm_taglist_init(&tl);
	int retval = hm_taglist_push(&tl, tag1, NULL);
	dbgassert(retval == 0);
	hm_taglist_add_ignored(&tl, 6); // pretend a macro got expanded
	retval = hm_taglist_push(&tl, tag2, NULL);
	dbgassert(retval == 0);

	// actual tests:
	
	hm_tag curtag = hm_taglist_peek(&tl);
	dbgassert(cu_str_eq(cu_str_from_cstr("tag two"), curtag.txt));
	dbgassert(curtag.row == 1);
	dbgassert(curtag.col == 1);
	dbgassert(curtag.n_to_ignore == 0);
	dbgassert(curtag.tag_len == s2.len);

	hm_taglist_advance(&tl, (cu_str){
		.buf = s2.buf,
		.len = 13
	});
	curtag = hm_taglist_peek(&tl);
	dbgassert(cu_str_eq(cu_str_from_cstr("tag two"), curtag.txt));
	dbgassert(curtag.row == 2);
	dbgassert(curtag.col == 4);
	dbgassert(curtag.n_to_ignore == 0);
	dbgassert(curtag.tag_len == s2.len - 13);

	hm_taglist_advance(&tl, cu_str_from_cstr("ond fileasdf"));
	curtag = hm_taglist_peek(&tl);
	dbgassert(cu_str_eq(cu_str_from_cstr("tag one"), curtag.txt));
	dbgassert(curtag.row == 1);
	dbgassert(curtag.col == 1);
	dbgassert(curtag.n_to_ignore == 2);
	dbgassert(curtag.tag_len == s1.len);

	hm_taglist_advance(&tl, cu_str_from_cstr("ghtext from\n"));
	curtag = hm_taglist_peek(&tl);
	dbgassert(cu_str_eq(cu_str_from_cstr("tag one"), curtag.txt));
	dbgassert(curtag.row == 2);
	dbgassert(curtag.col == 1);
	dbgassert(curtag.n_to_ignore == 0);
	dbgassert(curtag.tag_len == s1.len - 10);

	hm_taglist_advance(&tl, cu_str_from_cstr("first file\n"));
	dbgassert(hm_taglist_len(&tl) == 0);

	hm_taglist_free(&tl, NULL);
}

int main(void)
{
	test_hm_buf();
	test_hm_tag();
}
