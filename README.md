# Perfect Tower Script Editor

See it in action at https://d0sboots.github.io/perfect-tower!

This is a fork of the original editor. However, I've added several features that the maintainer didn't want,
or were too large of a change to include upstream:

* Support for lua() and len() macro functions. lua(), in particular, allows you to do arbitrarily-complex preprocessing inside your code,
eliminating the need for out-of-band data processing scripts.

I've also committed several major improvements upstream. If these are sufficient for your needs, consider using the base editor (https://kyromyr.github.io/perfect-tower),
which will likely be more actively maintained:

* Support for macro functions. Not quite as powerful as lua(), but they can do a whole lot more than basic macros, letting you build complicated
text-replacement schemes.
* Backslash-continued lines. A line ending with a backslash will be considered a single line, so you don't have to keep scrolling right!
