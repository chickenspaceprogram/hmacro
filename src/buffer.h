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
#include <assert.h>
#include <cu/string.h>

typedef struct {
	uint8_t *buf;
	uint64_t capacity;
	uint64_t start;
} hm_buf;

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
int hm_buf_reserve(hm_buf *buf, uint64_t nchrs, cu_alloc *alloc);
static inline int hm_buf_push(hm_buf *buf, cu_string_view txt, cu_alloc *alloc)
{
	if (txt.buf == NULL)
		return 0;
	int retval = hm_buf_reserve(buf, buf->capacity - buf->start + txt.len, alloc);
	if (retval != 0)
		return retval;
	buf->start -= txt.len;
	memcpy(buf->buf + buf->start, txt.buf, txt.len);
	return 0;
}
static inline void hm_buf_pop(hm_buf *buf, uint64_t nchrs_popped)
{
	buf->start += nchrs_popped;
	assert(buf->start <= buf->capacity);
}
static inline cu_string_view hm_buf_contents(hm_buf *buf)
{
	assert(buf->buf != NULL);
	return (cu_string_view){
		.buf = buf->buf + buf->start,
		.len = buf->capacity - buf->start,
	};
}

typedef struct {
	// Text of the tag (usually a filename
	cu_string_view tag;
	// Current row and col (set these to 1 and 1
	uint64_t row;
	uint64_t col;

	// Chars to consume before incrementing row/col
	uint64_t n_to_consume;

	// number of chars to consume while advancing row/col before removing tag
	uint64_t chars_in_tag;
} hm_tag;

#define HM_TAG_DEFAULT(TAGTXT) (hm_tag){\
	.tag = (TAGTXT),\
	.row = 1,\
	.col = 1,\
	.n_to_consume = 0,\
}

typedef struct {
	hm_tag *buf;
	uint64_t capacity;
	uint64_t nel;
} hm_taglist;


static inline void hm_taglist_init(hm_taglist *tl)
{
	tl->buf = NULL;
	tl->capacity = 0;
	tl->nel = 0;
}
static inline void hm_taglist_free(hm_taglist *tl, cu_alloc *alloc)
{
	cu_freearray(tl->buf, tl->capacity, sizeof(hm_tag), alloc);
}
int hm_taglist_reserve(hm_taglist *tl, uint64_t ntags, cu_alloc *alloc);
static inline int hm_taglist_push(hm_taglist *tl, hm_tag tag, cu_alloc *alloc)
{
	int retval = hm_taglist_reserve(tl, tl->nel + 1, alloc);
	if (retval != 0)
		return retval;
	tl->buf[tl->nel++] = tag;
	return 0;
}
static inline hm_tag hm_taglist_pop(hm_taglist *tl)
{
	assert(tl->nel != 0);
	return tl->buf[--tl->nel];

}
void hm_taglist_advance(hm_taglist *tl, cu_string_view text);

// use when pushing expanded stuff onto the buffer to protect the top tag
static inline void hm_taglist_add_consumable(hm_taglist *tl, uint64_t nconsume)
{
	assert(tl->nel > 0);
	tl->buf[tl->nel - 1].n_to_consume += nconsume;
}
static inline hm_tag hn_taglist_peek(hm_taglist *tl)
{
	return tl->buf[tl->nel - 1];
}
