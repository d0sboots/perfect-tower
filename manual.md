# AI Editor Manual

The external editor is compiler for TPT2 AI scripts. It closely follows the
structure of the in-game AI language, while adding lots of nice convenience
features like named variables, loop labels, macros, comments, and more.

Since most of the nice features *cannot* be represented in-game, it is
recommended that you keep the "master copy" of your script as text, and
repeatedly export it to test new versions. However, there is an import feature
that allows you to work in a back-and-forth way.

## Types

The language has 5 types: `bool`, `int`, `double`, `vector`, and `string`.

* `bool` is either `false` or `true`.
* `int` is a 32-bit signed integer. They are represented as plain numbers.
   There is no 0x hexidecimal notation or octal notation, but leading zeros
   are allowed (and ignored).
* `double` is a 64-bit double-precision floating-point value. Double literals
  are distinguished from ints by having a decimal point, or using scientific
  notation. `100.` (trailing period) is allowed, but `.99` (leading period) is *not*.
* `vector` is a pair of *single-precision* values. The functions for accessing
  the parts return `double`s, but be aware of the precision loss. There are no
  vector literals, they can only be made with functions!
* `string` is a string where each character is a UTF-16 codepoint. The details
  are the same as for C#, which is the internal implementation.
  https://learn.microsoft.com/en-us/dotnet/standard/base-types/character-encoding-introduction
  Strings can be up to 2 billion characters long, but are truncated to 65536
  characters when stored in a variable. This does mean you can operate on
  longer string tables, if you keep them as literals. Strings literals can be
  delimited with either `"` or `'`, and support the following backslash escape sequences:
  * `\b` - backspace (char 0x08)
  * `\f` - formfeed  (char 0x0c)
  * `\n` - newline   (char 0x0a)
  * `\r` - carr. ret (char 0x0d)
  * `\t` - tab       (char 0x09)
  * `\v` - vert tab  (char 0x0b)
  * `\\` - backslash (char 0x5c)
  * `\x` - hex escape (exactly 2 hex chars follow)
  * `\u` - unicode escape (exactly 4 hex chars follow)
  * `\U` - long unicode escape (exactly 6 hex chars follow)
  Note that the `\x` escape creates raw bytes, which might not form valid
  utf-8, and the game expects utf-8. For instance, `"↑"`, `"\u2191"` and
  `"\xe2\x86\x91"` are all the same thing. Also, for annoying technical
  reasons there aren't short escapes for `\'` or `\"`. Use `\x27` and `\x22`.

## Language conventions

At its most basic, everything is represented by function-calls: An
*identifier* that matches `[a-zA-Z_][a-zA-Z0-9._]*` for the function name,
followed by a series of *arguments* enclosed by parenthesis. (Unicode can also
be used in any identifier, although the built-in function calls never use it.)
For example:

```
local.double.set("foo", 1.5)
```

All the arguments will be one of the 5 types, and usually the return value
will be as well. If the function does not return anything, then it is a
*statement* and can only be used at the top-level of a line. All of the
functions that change state (set variables, click buttons, or otherwise do
something in the game) are statements.

A program is made up of a number of *lines*, with each line having a statement
as its root. Lines are important because you are limited in how many you can
have: There is a strict limit of 50.

## Syntax shortcuts

The editor provides "syntactical sugar" that makes programming more pleasant.
These translate directly to underlying AI functions. In some cases, it's good
to know the translations, because the game functions can be more general.

* The arithmetic operators `+`, `-`, `*`, `/`, `%`, `^` and `//`. `%` is
  modulus, `^` is exponent and `//` is log-with-base (*not* floor-division)!
  These all work for both ints and doubles. Additionally there are `%&`, `%^`
  and `%|` which are bitwise-and, xor and or, defined only for ints.
  Note that the game does not have unary negation, so the editor does not
  either: You can specify negative constants, or subtract from 0 in the
  extreme case. These translate to `arithmetic.int()` or
  `arithmetic.double()`, with the middle argument being
  (correspondingly) "+", "-", "*", "/", "mod", "pow", "log", "and", "xor" and "or".
* `.` is used for string concatenation, which is the `concat()` function. It
  also automatically coerces int and double arguments to strings using `i2s()`
  and `d2s()`.
* The comparison operators `==`, `!=`, `<`, `<=`, `>`, `>=`, `&&` and `||`.
  These work for string, double, int, and bool although not universally: The
  `>`/`<` comparisons only work for numbers, and `&&`/`||` only work for bools.
  Vectors do not support comparison directly, you must compare their parts.
  These are translated into `comparison.<type>()`, with the middle argument
  being (correspondingly) "==", "!=", "<", "<=", ">", ">=", "&&" and "||".
* The assignment operator `=`, which is used with variables. It translates
  into `<local/global>.<type>.set(<variable>, <value>)`. There is also "+="
  notation, which translates directly into "variable = variable + value". It
  works for *any* arithmetic operator, even weird ones like `//=`.

The precedence of operators is as follows (items on the same line have
the same precedence, and are grouped left-to-right):
* `^` `//`
* `*` `/` `%`
* `+` `-`
* `%&`
* `%^`
* `%|`
* `.`
* `==` `!=` `<` `<=` `>` `>=`
* `&&`
* `||`
* `=` `+=` etc.

### Constant folding

Arithmetic and string concatenation is *constant folded*, which means that if
both arguments are constants then it will be replaced (at compile-time) with
the computed result. For instance, the statement:
```
:global string a
a = "Foo = " . 10^(50 // 3) + 3 * 4
```
will be translated to `global.string.set("a", "Foo = 1012")`; none of the original
expression remains in the exported script.

Usually this is a good thing (it saves time and space in your resulting
scripts), but if you want to prevent this you can use the underlying functions
(`concat()`, `arithmetic.int()`, and `arithmetic.double()`) which are not
constant-folded.

Also note that the folding is not as smart as with optimizing compilers; it
only works on expression nodes where both parts are constant. So `10 + a + 10`
cannot be folded, since that is actually `(10 + a) + 10` and `10 + a`
contains a non-constant term. Similarly (and surprisingly), `a + 10 + 10` will
*not* be folded, because it's `(a + 10) + 10`. You have to manually introduce
parenthesis or rearrange the expression if you want folding to apply.

## Variables

Variables are declared with the syntax `:<local/global> <type> <identifier>`.
For instance:
```
:global string foo
```

Local variables are *local* to a particular script (instance): Every copy of
the script sees a fresh copy. Global variables exist with a single version
*globally*, even when the script isn't running, and show up in the "Variables
(Global)" list in the top-right in-game.

The variable declaration does not produce any code; what it does is tell the
compiler to translate *assignment expressions* into
`<local/global>.<type>.set()` and other uses of the variable name into
`<local/global>.<type>.get()`. You can use these functions directly instead,
and it is often useful to do so: For instance, by appending an index to the
name, you can create an array-like construct:
```
lis("my_array" . idx, value)
```
(As shorthand, `local.int.set` can be shortened to `lis`, and the same scheme
for the other get/set functions. Note that the expanded form for
`lvs`/`lvg`/`gvs`/`gvg` is `<local/global>.vec2.<set/get>`, due to game
internal quirks.

All variables are "initialized" to 0/false/"". In other words, if you try to
read from a variable that hasn't been set yet, you will get
false/0/0.0/""/(0.0,0.0), depending on the type. Uninitialized variables are
not "null", since there are no null values in this language.

There is also a const declaration: `:const <type> <name> <value>`. This is
very similar to using a macro to substitute values, except that it is
type-checked.

Note that since variables names are `<identifier>`s, there is great freedom in
what you can use, but not *everything* is valid. If you want to use spaces or
other reserved characters in a variable (which the game allows), you will need
to use the set/get functions directly. Also note that variables (like
everything) are case-sensitive, but the game font does not distinguish between
cases very well.

## Loops and Labels

The language does not have the traditional control-flow statements like "for
loops", "if blocks", etc. Instead, it has `goto()`, and its slightly more
convenient cousin `gotoif()`.

`goto(x)` jumps to line x, where x is an integer. `gotoif(x, <condition>)`
does the same, but only if <condition> is true. Both of these are statements,
so they take up a line, but you can use an expression for "x" to do computed
gotos to save lines.

Counting line numbers is a pain, especially when adding or removing lines. To
avoid this, use labels:
```
loop:
i += 1
gotoif(loop, i < 10)
```
Labels act like integer constants, and always evaluate to the line number of
the next statement. Since they are constants, you can use them in expressions,
do math on them, etc.

## Comments and Style

You can add comments to your code with `;`, and are encouraged to do so!
Comments continue until the end of line.
```
a = (a^2.)^0.5  ; Get the absolute value by taking the square-root of the square
```

Worth noting is another feature for organizing your code:
backslash-continuation. If a backslash appears at end-of-line, it means the
line continues on instead of stopping.
```
foo = if(\
  condition,\
  foo . ", and another thing: " . (i + 20),\
  foo\
)
```
However, comments continue until the end of the line, and backslashes prevent
the line from ending. So you can't use comments inside an expression that has
been broken up like this, because it would comment out the remainder of the
expression. This is an unfortunate limitation that I hope to lift someday.

## Compiler Directives

You've already seen the most important directives: `:local`, `:global` and
`:const`. All directives start with colon:

`:budget_cap <value>` sets the budget cap for the script. The special value
"max" means -1, which the game interprets as limited only by upgrades in the HQ.
0 means the old one-line-per-frame execution style, 1 acts like the old
"atomic functions," and higher indicate a cost budget that is used before
execution moves on to the next script (if any). Most expensive functions cost 100.

If this directive is *not* set, then (for compatibility reasons) no budget will
be set in the exported script, which means a default of 0 (the old
one-line-per-frame behavior).

`:use_budget [true/false/default]` sets the "use budget" checkbox. You usually
don't need this directive, since setting a :budget_cap will automatically
enable this, and if you leave off :budget_cap then this will also be ommitted,
giving the default behavior of unset. You can use this to be explicit, or to
unset the checkbox while still setting a value with :budget_cap. `:use_budget
default` means to explicitly *not* set a value for the checkbox, and is mostly
useful for testing the editor.

`:name <name>` sets the export name for the current script. By default, the
name comes from whatever it is called in the left sidebar, but this can be
changed. In particular, macros can be used to give everything a uniform
package.

Scripts use the syntax "package:script_name" in their names to separate the
package from the script part. Scripts without a package will appear "loose,"
at the top of the scripts list, while packaged scripts appear together in
their own groups. It used to be important to use the full
"package:script_name" value to refer to scripts when using things like
`execute()` and `stop()`; this was eventually changed in-game so that leaving
off the package will try matching inside the current package before matching
against loose scripts.

`:import <script>` imports a script by its sidebar name. It doesn't matter
what workspace the scripts are in, and if there are multiple scripts with the
same name only the first will get imported. (This is confusing, so it's not
recommended.)

A script used for import must have no lines, impulses or conditions. In that
case, you might wonder what it is good for? Import scripts exist primarily to
set up macro definitions, which can then be shared and used by multiple other
scripts. When exporting a workspace, scripts with no lines are skipped, so
import scripts won't be included in the export.

## Macros

The language includes a powerful macro system to make writing complicated or
repetitive code easier.

A simple macro is defined using `#macroname substitution text` and invoked
with `{macroname}`. The simple form will perform simple text substitution,
replacing the `{macroname}` invocation with `substitution text`.

You can define a macro with arguments via `#macroname(arg1, arg2) substitution {arg1} {arg2} text`.
They are invoked with `{macroname(param1,param2)}`. All occurences of the
`{arg1}` and `{arg2}` tags in the definition will be replaced appropriately,
and then the whole result substituted in.

Note that although you can use optional space between the arguments in the
macro definition, any space in the *invocation* will become part of the
parameter(s) that are substituted. Often this does not matter, but it's worth
knowing.

A *simple* macro call is limited to one line - if you write `{macroname` as
the full line, it will go through verbatim. However, macro functions can
continue onto multiple lines - `{macroname(` will grab successive lines until
it finds the closing `}`. In addition, if the opening `(` or `,` that starts a
new param is immediately followed by a newline, the newline will not become
part of the param, as if there was a `\` present. For instance:
```
{macroname(
arg1,
arg2)}
```
is the same as `{macroname(arg1,arg2)}`. Note that otherwise, newlines will be
passed through and substituted into the macro, which may not be what you want.

To be able to define (and not just call) macros using multipile lines, use
this braced-definition syntax:
```
#macroname={
  statement1()
  statement2()  ; A comment about what's happening
} ; Trailing comments are also possible
```

Braced definitions start with `={` and end at the matching `}`. They can be
single line or multiple. Like with macro calls, if the `={` is immediately
followed by a newline, the newline is ignored.

Note that a multiline macro definition will expand to multiple actual lines;
you can use this to have one macro expand to multiple statements.

Macros can contain other macros, both in their definitions and invocations.
This is legal:
```
#concat(a, b) {a}{b}
{co{concat(nc,at)}(foo,bar)}
```
Yielding "foobar".

Because macro processing runs before other processing, it can also be used to
define variables, constants, set other compiler directives, or even (with
appropriate quoting) define macros.

There are a handful of "special" macros that are hardcoded and predefined:
* `{}` returns `{}`
* `{[}` returns `{`
* `{]}` returns `}`
* `{(}` returns `(`
* `{)}` returns `)`
* `{,}` returns `,`. These 5 are used to escape macro special characters from macro processing.
* `{len(...)}` returns the length in characters of whatever is contained within.
* `{lua(...)}` evaluates a lua expression and returns the stringified result.
  This can be used for arbitrarily-complicated metaprogramming.

The `len` and `lua` macros use a special argument-processing mode known as
"rawarg" mode. Normal macro calls look for commas to separate macro
arguments, and use nested parentheses to protect against stray commas. For
instance: `concat(count("dust", 2), + 15.0)` would produce
`count("dust", 2) + 15.0`, and the first comma would not separate arguments
because it is within nested parentheses. However, this creates a requirement
that parentheses in the macro call be balanced.

For `lua` code especially this is annoying, which is where "rawarg" mode comes
in. The entire single argument starts after the `{(` and ends at the first `)}`,
and that's it. If you want this behavior for one of your own macros, you can
prefix the argument name with `$`: `#macroname($arg) definition`. By
necessity, there can only be one argument in this case.
