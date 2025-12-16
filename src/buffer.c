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

#include "buffer.h"
#define FILL_FACTOR 2
#define MIN_CAPACITY 16

static inline uint64_t next_pwr_2(uint64_t val)
{
	--val;
	val |= val >> 1;
	val |= val >> 2;
	val |= val >> 4;
	val |= val >> 8;
	val |= val >> 16;
	val |= val >> 32;
	++val;
	return val;
}

int hm_buf_reserve(hm_buf *buf, uint64_t nchrs, cu_alloc *alloc)
{
	uint64_t next_cap = next_pwr_2(nchrs);
	if (next_cap <= buf->capacity)
		return 0;
	if (next_cap < MIN_CAPACITY)
		next_cap = MIN_CAPACITY;
	uint8_t *new_buf = cu_malloc(next_cap, alloc);
	if (new_buf == NULL)
		return -1;
	uint64_t txt_len = buf->capacity - buf->start;
	memcpy(new_buf + next_cap - txt_len, buf->buf + buf->start, txt_len);
	cu_free(buf->buf, buf->capacity, alloc);
	buf->buf = new_buf;
	buf->capacity = next_cap;
	buf->start = next_cap - txt_len;
	return 0;
}
int hm_taglist_reserve(hm_taglist *tl, uint64_t ntags, cu_alloc *alloc)
{
	uint64_t next_cap = next_pwr_2(ntags);
	if (next_cap <= tl->capacity)
		return 0;
	hm_tag *new_buf = cu_reallocarray(
		tl->buf,
		next_cap,
		tl->capacity,
		sizeof(hm_tag),
		alloc
	);
	if (new_buf == NULL)
		return -1;
	tl->buf = new_buf;
	tl->capacity = next_cap;
	return 0;
}
void hm_taglist_advance(hm_taglist *tl, cu_string_view text)
{
	while (text.len > 0) {
		assert(tl->nel != 0);
		if (tl->buf[tl->nel - 1].n_to_consume > 0) {
			if (tl->buf[tl->nel - 1].n_to_consume > text.len) {
				tl->buf[tl->nel - 1].n_to_consume -= text.len;
				return;
			}
			else {
				text.len -= tl->buf[tl->nel - 1].n_to_consume;
				text.buf += tl->buf[tl->nel - 1].n_to_consume;
				tl->buf[tl->nel - 1].n_to_consume = 0;
			}
			continue;
		}
		uint64_t tag_chrs =
			text.len > tl->buf[tl->nel - 1].chars_in_tag ?
			tl->buf[tl->nel - 1].chars_in_tag :
			text.len;
		void *nl = memchr(text.buf, '\n', tag_chrs);
		if (nl == NULL) {
			text.buf += tag_chrs;
			text.len -= tag_chrs;
			tl->buf[tl->nel - 1].col += tag_chrs;
			tl->buf[tl->nel - 1].chars_in_tag -= tag_chrs;
		}
		else {
			uint64_t chrs_popped = (uint8_t *)nl + 1 - text.buf;
			++tl->buf[tl->nel - 1].row;
			tl->buf[tl->nel - 1].col = 1;
			tl->buf[tl->nel - 1].chars_in_tag -= chrs_popped;
			text.buf += chrs_popped;
			text.len -= chrs_popped;
		}

		if (tl->buf[tl->nel - 1].chars_in_tag == 0)
			hm_taglist_pop(tl);
	}
}
