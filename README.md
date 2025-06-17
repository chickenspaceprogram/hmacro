# hmacro

A kinda-terrible macro expander originally written in Haskell, but later
rewritten in Rust.

## Build

You can build this with Cargo, just run `cargo build`.

You can also install it with `cargo install`.

## Invoking from the command line
~~~
Usage:

hmacro [OPTIONS] <filename>

`-v', `--version' - Print a version message, then exit
`-h', `--help'    - Print a help message, then exit
`--license'       - Display information about hmacro's license
`-Dmacro=exp'     - Predefines a new macro, named `macro', that expands
                    to `exp'.

You can pass a list of filenames to hmacro.
They will each be expanded separately and the results concatenated.
~~~

## Example CLI invocations

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
~~~

Practically, `hmacro` source consists of macros and scopes. 
Any macros defined inside a scope are no longer valid once outside of that scope, however, any scopes defined inside a scope inherit macros defined in scopes above that scope. 
Scopes are delineated with curly braces, `{}`.
Curly braces can be escaped with `\{` and `\}`.

A macro looks something like `\name`, where name is a name that must start with an alphabetic character, `-`, or `_`, and must consist only of alphanumeric characters, as well as `-` or `_`. 
The `\` character can be escaped with `\\`.
If a macro is immediately followed by one or more scopes, those scopes are considered arguments to that macro and are available to it when it expands.


Some macros are predefined as follows:

- `\def{name}{expansion}`
Defines a new macro called `name` that expands to `expansion`.
`expansion` consists of text interspersed with macro arguments.
A macro argument is of form `$num`, where `num` is the number of
the argument.
`$1` is the first argument, `$2` is the second, and so on.
If a `$` character is followed by a numeric character, and you do not want
either to be treated as specifying an argument, escape the `$` with `\$123`.
- `\include{filename}`
Expands to the text of `filename`.
The file itself will be processed for hmacro scopes and macros.
Any macros that are not within a scope will be defined.
Recursive includes are not allowed.
- `\incldefs{filename}`
Expands to nothing, but any macros in the file `filename` that are outside of
a scope will be defined after this macro is called.
- `\cat{arg1}{arg2}...{argn}`
Concatenates all arguments provided without expanding any of them.


