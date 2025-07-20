# hmacro - a macro expander
# Copyright (C) 2025 Athena Boose

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# SPDX-License-Identifier: GPL-3.0-or-later

cmake_minimum_required(VERSION 3.21)
project(hmacro CXX)

# this file does kinda suck but it also does work and i can't be bothered to make it decent

option(HMACRO_USE_ASAN "Links c-utils and any of its tests with the ASAN library.")
option(HMACRO_DBGFLAGS "Adds extra GCC/Clang-specific compiler flags.")
option(HMACRO_CLANG_TIDY "Uses the clang-tidy tool to statically analyze source code when building.")
option(HMACRO_TESTS "Builds c-utils's unit tests.")

set(CMAKE_CXX_STANDARD 23)

if(HMACRO_TESTS)
	include(CTest)
endif()


if(HMACRO_USE_ASAN)
	message(NOTICE "Option `HMACRO_USE_ASAN` specified. Linking ASAN and setting `HMACRO_DEBUG`.")
	set(CMAKE_CXX_FLAGS "-fsanitize=address -fsanitize-recover=address -fsanitize=undefined -fsanitize=leak")
	set(HMACRO_DEBUG ON)
endif()

if (HMACRO_DBGFLAGS)
	message(NOTICE "Option `HMACRO_DBGFLAGS` specified. Adding extra compile flags.")
	set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wpedantic -Werror")
endif()

if (HMACRO_CLANG_TIDY)
	find_program(CLANG_TIDY clang-tidy)
	if (CLANG_TIDY)
		message(NOTICE "Found clang-tidy!")
		list(APPEND CMAKE_C_CLANG_TIDY
			"--use-color"
		)
		set(CMAKE_C_CLANG_TIDY ${CLANG_TIDY})
	else()
		message(NOTICE "clang-tidy not found and not used.")
	endif()
endif()



function(make_test testfilename) # can take libs to link as an optional arg
if (HMACRO_TESTS)
	get_filename_component(EXENAME ${testfilename} NAME_WLE)
	add_executable(${EXENAME} ${testfilename})
	target_link_libraries(${EXENAME} ${ARGN})
	add_test(
		NAME ${EXENAME}
		COMMAND $<TARGET_FILE:${EXENAME}>
	)
endif()
endfunction()

add_subdirectory(src)
