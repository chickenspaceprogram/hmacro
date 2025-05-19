# hmacro

A kinda-terrible macro expander written in Haskell

## Build

Build this by running the shell script `build.sh`. The intermediate files by default get put in `build`, and the executable is located at `build/hmacro`. You can then install it if you want by moving it to somewhere on your path.

To build this you must have GHC installed. Your Unix distribution probably has a package. I've tested this with GHC v9.12.2 at time of writing, but I may test other versions in future. If you encounter errors with other GHC versions please raise an issue and I'll attempt to fix it.

For now this is only supported on Unix, but it should be pretty simple to compile on Windows as well. Just run GHC on `hmacro.hs` and GHC should take care of the rest. Some optimization flags (`-O2`) are probably not unwise as well.


## Invoking from the command line
~~~
hmacro [OPTIONS] <filename> [-o OUTFILE]
~~~

`-v`, `--version` - Print a version message, then exit
`--help` - Print a help message, then exit

`-o`, `--output` - Specify a specific filename to output to (default is to write output to stdout)

You can pass a list of filenames to hmacro. They will each be expanded separately and the results concatenated.

## Example CLI invocations

~~~
hmacro foo.hm bar.hm -o baz.txt
~~~

Expands `foo.hm` as well as `bar.hm`, then concatenates the results together and outputs them to baz.txt.

~~~
hmacro foo.hm
~~~

Expands `foo.hm`, then prints the result to stdout.

## Source file format

`hmacro` works similarly to `m4` or the C preprocessor. It takes some text containing macros and definitions of those macros, and then expands those macros as desired and outputs the result.

Macros generally follow the vaguely-EBNF-style format below. Anything enclosed in `/` characters is a regular expression.

~~~
text ::= { textelem }
textelem ::= macro | scope | /[^\\]/ | "\\" | "\{" | "\}"
macro ::= '\' name { scope }
name ::= /[a-zA-Z\-_][a-zA-Z0-9\-_]*/
scope ::= '{' text '}'
~~~

Practically, `hmacro` source consists of macros and scopes. Any macros defined inside a scope are no longer valid once outside of that scope, however, any scopes defined inside a scope inherit macros defined in scopes above that scope. Scopes are delineated with curly braces, `{}`. 

A macro looks something like `\name`, where name is a name that must start with an alphabetic character, `-`, or `_`, and must consist only of alphanumeric characters, as well as `-` or `_`. If a macro is immediately followed by one or more scopes, those scopes are considered arguments to that macro and are available to it when it expands.

New macros can be defined using the `\def` macro. This macro takes two arguments
