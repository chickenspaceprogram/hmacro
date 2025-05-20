# hmacro

A kinda-terrible macro expander written in Haskell

## Build

Build this by running the shell script `build.sh`. 
The intermediate files by default get put in `build`, and the executable is located at `build/hmacro`. 
You can then install it if you want by moving it to somewhere on your path.

To build this you must have GHC installed. 
Your Unix distribution probably has a package. 
I've tested this with GHC v9.12.2 at time of writing, but I may test other versions in future. 
If you encounter errors with other GHC versions please raise an issue and I'll attempt to fix it.

For now this is only supported on Unix, but it should be pretty simple to compile on Windows as well. 
Just run GHC on `hmacro.hs` and GHC should take care of the rest. 
Some optimization flags (`-O2`) are probably not unwise as well.


## Invoking from the command line
~~~
Usage:

hmacro [OPTIONS] <filename> [-o OUTFILE]

`-v`, `--version` - Print a version message, then exit
`--help`          - Print a help message, then exit
`-o`, `--output`  - Specify a filename to output to (default: write to stdout)
`--license`       - Display information about hmacro's license

You can pass a list of filenames to hmacro.
They will each be expanded separately and the results concatenated.
~~~

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

`hmacro` works similarly to `m4` or the C preprocessor. 
It takes some text containing macros and definitions of those macros, and then expands those macros as desired and outputs the result.

Macros generally follow the vaguely-EBNF-style format below. Anything enclosed in `/` characters is a regular expression.

~~~
text ::= { textelem }
textelem ::= macro | scope | /[^\\]/ | "\\" | "\{" | "\}"
macro ::= '\' name { scope } | def | include
name ::= /[a-zA-Z\-_][a-zA-Z0-9\-_]*/
scope ::= '{' text '}'
def ::= '\def' scope scope
include ::= '\include' scope
~~~

Practically, `hmacro` source consists of macros and scopes. 
Any macros defined inside a scope are no longer valid once outside of that scope, however, any scopes defined inside a scope inherit macros defined in scopes above that scope. 
Scopes are delineated with curly braces, `{}`.
Curly braces can be escaped with `\{` and `\}`.

A macro looks something like `\name`, where name is a name that must start with an alphabetic character, `-`, or `_`, and must consist only of alphanumeric characters, as well as `-` or `_`. 
The `\` character can be escaped with `\\`.
If a macro is immediately followed by one or more scopes, those scopes are considered arguments to that macro and are available to it when it expands.

New macros can be defined using the `\def` macro. 
This macro takes two arguments; the first is the macro's name, and the second is a macro definition. 
Macro definitions are themselves expanded the same way as normal macro arguments; however, after they are expanded, `hmacro` will search the definition for argument-identifiers, which are denoted by the character `$` immediately followed by ASCII numeric characters. 
The base-10 number following `$` refers to the specific argument the argument-identifier will be replaced with, so `$0` is the first argument, `$1` is the second, and so on.
Argument-identifiers can be escaped with `\`, so `\$` will just expand to `$`.
If the `$` is not followed by numeric characters it is unnecessary to do this, though, since `$` only signifies an argument-identifier when immediately followed by numeric characters.

Files with predefined macros can be included into other files with the `\include` macro. 
This macro takes one argument, the filename of the file you wish to include.
As with all macros, the filename can be composed of whatever macro nonsense you want, it's all expanded prior to inclusion.
Due to a lack of planning when designing this application, files may only be included once.
If you need to include some actual text, it's probably best to define a macro within a file that expands to the desired text.
Although, include will include any non-macro text as expected.
