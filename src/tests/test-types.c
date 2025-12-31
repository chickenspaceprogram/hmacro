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

#include "../types.h"

char *good_examples[HM_NUM_FUND_TYPEIDS] = {
	" \t\n\v\f\r   \n\n\nbad text",
	"\\\\some more text",
	"*also some text",
	"$fsadfsafdsafsd",
	"-a negative number",
	"::this is a namespacelakfjdslkaf",
	"\\macro with some text",
	"!type with some text",
	":typealternate with some text",
	"{ inside here is a \\scope}",
	"this is just a chr",
	"12345 wow a number ;3",
};

int main(void)
{
}
