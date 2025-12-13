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
#include <cu/string.h>

typedef struct {
	uint8_t *buf;
	size_t capacity;
	size_t start;
} hm_buf;

typedef cu_string_view hm_tag;

typedef struct {
	cu_string_view *buf;
	size_t capacity;
	size_t nel;
} hm_taglist;

static inline void hm_buf_init(hm_buf *buf)
{
	buf->buf = NULL;
	buf->capacity = 0;
	buf->start = 0;
}
static inline void hm_buf_free(hm_buf *buf, cu_alloc *alloc)
{
	cu_free(buf->buf, buf->capacity, alloc);
}
int hm_buf_push(hm_buf *buf, cu_string_view txt, cu_alloc *alloc);
void hm_buf_pop(hm_buf *buf, size_t nchrs_popped);
static inline cu_string_view hm_buf_contents(hm_buf *buf)
{
	return (cu_string_view){
		.buf = buf->buf + buf->start,
		.len = buf->capacity - buf->start,
	};
}

void hm_taglist_init(hm_taglist *tl);
void hm_taglist_free(hm_taglist *buf, cu_alloc *alloc);
int hm_taglist_push(hm_taglist *tl, hm_tag tag, cu_alloc *alloc);
hm_tag hm_taglist_pop(hm_taglist *tl);
hm_tag hn_taglist_peek(hm_taglist *tl);
