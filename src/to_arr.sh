#!/bin/sh

# shell script used to compile the prelude
#
# the precompiled prelude is just included in the main repo because i want
# this to be portable to windows and that's the easiest way to do it

# you must have a POSIX-compliant shell (like Bash) and the program xxd to run this

# this only exists because #embed is brand new and the stupid way is portable

if [ -z $1 ]; then
	echo 'Must supply first argument!' 1>&2
	exit
fi

xxd -i $1 | tail -n +2 | head -n -2 | head -c -1
echo ', 0x00'
