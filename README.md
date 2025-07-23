# hmacro

A preprocessor primarily intended for processing plaintext.

## Build

Buildable with CMake.

## Invoking from the command line
~~~
Usage:

hmacro [Flags] [Options] <filenames>

Flags:

-v, --version            - Print a version message, then exit
-h, --help               - Print a help message, then exit
-l, --license            - Display information about hmacro's license
-P, --no-default-prelude - Disables the default Prelude and Epilogue.
-E, --keep-esc           - Disables postprocessing of escaped characters

Options:

-Dmacro;3;exp         - Defines a new macro, named "macro", that pops 3
--define=macro;3;exp    arguments from the stack and expands to "exp". The
                        macro is defined after the Prelude and before any
                        files are expanded.

-p <prelude>          - Adds the file <prelude> to the Prelude. All files are
--prelude=<prelude>     added after the default Prelude, if enabled.

-e <epilogue>         - Adds the file <epilogue> to the Epilogue. All files
--epilogue=<epilogue>   are added before the default Epilogue, if enabled.

-o <output>           - Writes output to the file <output>. If this option is 
--output=<output>       not specified, output is written to stdout.

You can pass a list of filenames to hmacro.
They will each be expanded separately and the results concatenated.

Passing -- as an argument will result in all arguments after it being treated
as files to read from.

Furthermore, passing - as an argument will make hmacro read from stdin.

hmacro doesn't play super nicely in shell pipes. It works fine, but the current
implementation buffers input until it ends, then parses and outputs it all at
once.
~~~

## Source file format

`hmacro` works similarly to `m4` or the C preprocessor. 
It takes some text containing macros and definitions of those macros, and then expands those macros as desired and outputs the result.

### Macros

Macros start with the `\` character and are followed by a macro-name.
Macro-names start with an ASCII alphabetic character, `-`, or `_`, and after
that can have an ASCII alphanumeric character, `-`, or `_`.
Essentially, they will match the following regular expression:

~~~
\\[a-zA-Z\-_][a-zA-Z0-9\-_]*
~~~

### Arguments

Arguments immediately follow a macro.
There are two ways to specify an argument.

First, you can specify an argument "lazily".
This means that any macros, arguments, etc. within that argument won't get
touched when the macro is expanded.
The argument's text will simply be copy-pasted to the desired location,
without any expansion or anything.

Second, you can specify an argument "greedily".
This means that any macros, arguments, etc. within that argument will be
expanded as they are encountered, prior to that argument getting substituted
into any macro definitions.

To specify an argument lazily, wrap it in curly braces (`{}`). That would look
like this:

~~~
\macro{lazily evaluated argument}
~~~

To specify an argument greedily, wrap it in square braces (`[]`). That would
look like this:

~~~
\macro[greedily evaluated argument]
~~~

To explain another way, when encountering a `{` after seeing a macro, `hmacro`
will search for the closing bracket and stash the text in-between the brackets
as an argument. When encountering a `[`, `hmacro` will continue expanding and
processing text as before until it sees the matching `]`. The matching `]`
might itself be part of another macro's expansion.

### The argument-stack

When arguments are encountered following a macro, they are pushed onto an
argument stack.

`hmacro` will try to parse new arguments as long as it can, gathering them
into a list. Once it can no longer find a new argument to parse, the list will
then be pushed onto the stack in reverse order. The macro will then get
expanded.

For example:

~~~
\macro{arg1}{arg2}{arg3}{arg4} and some more text over here
~~~

Assuming there was nothing on the argument-stack beforehand, the stack would
look like this when `\macro` gets expanded:

~~~
arg1
arg2
arg3
arg4
~~~

This means that, once the arguments are pushed onto the stack, the top element
on the stack is `arg1`, the second is `arg2`, the third is `arg3`, and so on.

Once a macro finishes expanding, it will pop some number of arguments off the
stack. Hereafter, this will be referred to as "taking" some number of
arguments, although the reader should note that this is not strictly correct.

Macros can be called with any number of arguments, just so long as they do not
attempt to access arguments that are not on the stack and do not attempt to
pop a number of arguments from the stack that is greater than the actual number
of arguments on the stack.

### Defining macros

To define a new macro, use the `\def` macro, as follows:

~~~
\def{macro}{nargs}{expansion}
~~~

`\def` takes 3 arguments. `macro` is the name of the new macro. `nargs` is the
number of arguments the macro will pop from the stack. `expansion` is the text
that the macro will expand to.

To refer to elements on the argument-stack from within the expansion of a
macro, use an argument-specifier. Argument-specifiers are prefixed with a `$`
character and are followed by an integer that specifies the index of the
argument.

For example:

~~~
\def{yeet}{2}{Here $0 is $1 a macro}
~~~

The above defines a macro, called `\yeet`, that pops 2 arguments from the
stack when it finishes expanding. It expands to the text "Here ", followed
by the first argument on the stack, followed by the text " is ", followed
by the second argument on the stack, followed by the text " a macro".

So,

~~~
\yeet{this is an arg}{woah, another arg}
~~~

expands to

~~~
Here this is an arg is woah, another arg a macro
~~~

It's important to note again that a macro can pop arguments from the stack
that it does not access and a macro can access arguments on the stack that
it does not pop. You could totally access `$2` from `\yeet` if you wanted,
and you could also not bother to access `$0` or `$1` if you didn't want to.

Once a macro expands, its expanded text will be prepended to the rest of the
text in the file, and macro processing will continue.

This means that a macro's expanded result-text will get run through the
preprocessor again.

### Escape characters

If you'd like to escape a character, prefix it with `\`.

The following characters can be escaped:

~~~
\
\n
$
{
}
[
]
~~~

If `\n` is escaped, it expands to nothing. This allows for adding lines to
files that won't expand to anything. This is useful for defining macros, since
you may not want spurious empty lines in your final output.

Escaped characters are sent through `hmacro` completely unchanged.
In fact, they are only translated to their associated characters by a
postprocessor. You can disable this postprocessor with the `-E` command-line
argument.

### Builtins

`hmacro` defines a number of builtins, which are described below.
Each only pops and/or accesses each of the arguments shown off the stack.

- `\sep`
    - Expands to nothing, accesses no arguments, and pops no arguments from
      the stack.\
      Has the special property that, when encountered, `hmacro` will not
      attempt to search for arguments. This means that `\sep` can be used to
      forcibly end another macro's search for arguments, as argument-searching
      will cease on encountering a character that is not `{` or `[`, and `\sep`
      itself will not attempt to find arguments.
- `\def{name}{nargs}{expansion}`
    - Defines a new macro, named `name`.\
      Any previous definitions are overridden.
    - `name` : The name of the macro.
    - `nargs` : The number of arguments the macro will pop from the stack when
      it finishes expanding.
    - `expansion` : The string that `\name` will expand to. Any
      argument-specifiers inside `expansion` will expand to the $nth argument
      on the stack.
- `\include{filename}`
    - Expands to the text located at `filename`.\
      Recursive includes will be detected and compilation will terminate with
      an error.
    - `filename` : The file to include. `filename` is treated as a path
      relative to the parent directory of the file currently being expanded,
      unless it's an absolute path, in which case it is treated as an absolute
      path.
- `\if{condition}{exp1}{exp2}`
    - Expands to `exp1` if `condition` is nonzero.\
      Expands to `exp2` if `condition` is zero.\
      Lazy expansion is very useful with this macro. Lazy expansion allows for
      recursive macros to work properly. If the expression containing the
      recursive call is greedily expanded, the recursive calls will get
      greedily expanded until you have a stack underflow. However, if those
      calls are in a lazily-expanded argument, they won't be evaluated until
      after the macro expands, and as such won't get expanded at all if the
      conditional is set to expand to the other expression.
    - `condition` : The condition for the if statement
    - `exp1` : The first expression
    - `exp2` : The second expression
- `\error`
    - Stops compilation and produces an error message to the standard error
      stream.\
      The error message will include a dump of the stack's contents at the
      time the `\error` was encountered, but its precise format is left
      unspecified.
- `\defined{name}`
    - Expands to `1` if `name` is the name of a defined macro or builtin.
      Otherwise, expands to `0`.
    - `name` : The macro name to check.
- `\size`
    - Expands to the current number of elements on the stack.
- `\suffix{n}{txt}`
    - Expands to `txt`, without the first `n` bytes. 
      Effectively, if `txt` is `len` bytes long, it is turned into the
      substring `[n, len)`.
      If `n` is larger than the number of bytes in `txt`, expands to nothing.
    - `n` : number of characters to remove
    - `txt` : string to find the suffix of
- `\prefix{n}{txt}`
    - Expands to `txt`, without the last `n` bytes.
      Effectively, if `txt` is `len` bytes long, it is turned into the
      substring `[0, len - n)`.
      If `n` is larger than the number of bytes in `txt`, expands to nothing.
    - `n` : number of characters to remove
    - `txt` : string to find the prefix of
- `\len{txt}`
    - Expands to the length of `txt`, in bytes.
    - `txt` : string to find the length of
- `\replace{from}{to}{haystack}`
    - Replaces any occurrences of the string `from` with the string `to` in
      `haystack`.
    - `from` : The string to search for
    - `to` : The string to replace with
    - `haystack` : The string to search inside
- `\reverse{str}`
    - Expands to `str`, with all the bytes in it reversed.
    - `str` : The string to reverse
- `\find{needle}{haystack}`
    - Expands to the index of the first occurrence of the substring `needle`
      in `haystack`. If `needle` is not found, expands to `\len{haystack}`.
    - `needle` : The string to search for
    - `haystack` : The string to search in
- `\rfind{needle}{haystack}`
    - Expands to the index of the last occurrence of the substring `needle`
      in `haystack`. If `needle` is not found, expands to `\len{haystack}`.
    - `needle` : The string to search for
    - `haystack` : The string to search in
- `\findany{needle}{haystack}`
    - Expands to the index of the first occurrence of any of the characters
      in `needle` in `haystack`. If `needle` is not found, expands to 
      `\len{haystack}`.
    - `needle` : The characters to search for
    - `haystack` : The string to search in
- `\rfindany{needle}{haystack}`
    - Expands to the index of the last occurrence of any of the characters in
      `needle` in `haystack`. If `needle` is not found, expands to
      `\len{haystack}`.
    - `needle` : The characters to search for
    - `haystack` : The string to search in
- `\findnone{needle}{haystack}`
    - Expands to the index of the first occurrence of any of the characters not
      in `needle` in `haystack`. If `haystack` only contains the characters in
      `needle`, expands to `\len{haystack}`.
    - `needle` : The characters to not search for
    - `haystack` : The string to search in
- `\rfindnone{needle}{haystack}`
    - Expands to the index of the last occurrence of any of the characters not
      in `needle` in `haystack`. If `haystack` only contains the characters in
      `needle`, expands to `\len{haystack}`.
    - `needle` : The characters to not search for
    - `haystack` : The string to search in
- `\streq{arg1}{arg2}`
    - Expands to `1` if each byte of `arg1` and `arg2` is equivalent.\
      Expands to 0 otherwise.
- `\eq{arg1}{arg2}`
    - Expands to `1` if `arg1` and `arg2` are equivalent numbers.\
      Expands to 0 if they are not equivalent numbers, and terminates
      with an error if `arg1` or `arg2` are not numbers.
- `\gt{arg1}{arg2}`
    - Expands to `1` if `arg1` is a number strictly greater than `arg2`.\
      Expands to 0 if this is not the case, and terminates with an error if
      `arg1` or `arg2` are not numbers.
- `\nand{arg1}{arg2}`
    - If both `arg1` and `arg2` are nonzero, expands to 0.\
      Otherwise, expands to 1, and terminates with an error if
      `arg1` or `arg2` are not numbers.
- `\isnum{arg}`
    - If `arg` can be parsed as a number, expands to 1.\
      Otherwise, expands to 0, and terminates with an error if
      `arg1` or `arg2` are not numbers.
- `\add{num1}{num2}`
    - Adds 
- `\mult{num1}{num2}`
- `\div{num1}{num2}`

### Prelude

The Prelude is a bit of `\hmacro` source that is expanded and processed by
`\hmacro` before each file is processed.

You can define your own preludes, if you want, and use command-line arguments
to add them to a compilation, but `\hmacro` also has a "standard prelude" that
is automatically included.

### Epilogue

The Epilogue is the same as the Prelude, it's just included after each file.
This allows for the Prelude to include the entire file as an argument to a
macro, if that's desired.

The Standard Epilogue is currently empty as of the current version of hmacro,
but nonstandard Epilogues work fine.

