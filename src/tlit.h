#pragma once
#include <stdint.h>
#include <stddef.h>

// hmacro TransLITerator
//
// (i suck at acronyms)
//
// this takes a stream of octets and converts it into a stream of different octets

typedef struct {
	uint8_t lut[0x100];
} hmacro_tlit_lut;

static inline uint8_t
hmacro_tlit_octet(
	const hmacro_tlit_lut *lut,
	uint8_t chr
) {
	return lut->lut[chr];
}

static inline void
hmacro_tlit_stream(
	const hmacro_tlit_lut *lut,
	uint8_t *outbuf,
	const uint8_t *inbuf,
	size_t bufsz
) {
	for (size_t i = 0; i < bufsz; ++i) {
		outbuf[i] = lut->lut[inbuf[i]];
	}
}

typedef enum {
	HMACRO_NUMERIC_0,
	HMACRO_NUMERIC_1,
	HMACRO_NUMERIC_2,
	HMACRO_NUMERIC_3,
	HMACRO_NUMERIC_4,
	HMACRO_NUMERIC_5,
	HMACRO_NUMERIC_6,
	HMACRO_NUMERIC_7,
	HMACRO_NUMERIC_8,
	HMACRO_NUMERIC_9,
	HMACRO_TEXT,
	HMACRO_EXPANDER,
	HMACRO_QUOTER,
	HMACRO_MACRO_START,
	HMACRO_BEGIN_SCOPE,
	HMACRO_END_SCOPE,
	HMACRO_MACRO_NAME,
	HMACRO_WHITESPACE,
} hmacro_chartypes;

void hmacro_tlit_set_default(hmacro_tlit_lut *lut);

