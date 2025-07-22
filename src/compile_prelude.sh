#!/bin/sh

# shell script used to compile the prelude
#
# the precompiled prelude is just included in the main repo because i want
# this to be portable to windows and that's the easiest way to do it

# you must have a POSIX-compliant shell (like Bash) and the program xxd to run this

# this only exists because #embed is brand new, this is portable

{ xxd -i prelude.hm | tail -n +2 | head -n -2 | head -c -1; echo ', 0x00'; } > prelude.xxd.hm
