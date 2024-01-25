# Perfect Tower Script Editor

See it in action at https://d0sboots.github.io/perfect-tower!

This is a fork of the original editor. Since then, the original author (Kyromyr) moved on and stopped maintaining theirs, so this is the "official" editor now!

Advanced features that have been added along the way:

* Use of Web Workers to compile code, which greatly improves loading time and editor responsiveness.
* Support for `lua()` and `len()` macro functions. `lua()`, in particular, allows you to do arbitrarily-complex preprocessing inside your code,
eliminating the need for out-of-band data processing scripts.
* Added the :import directive to allow sharing common macros between multiple scripts.
* Support for macro functions. Not quite as powerful as `lua()`, but they can do a whole lot more than basic macros, letting you build complicated
text-replacement schemes.
* Backslash-continued lines. A line ending with a backslash will be considered a single line, so you don't have to keep scrolling right!
* Import/Export of whole workspaces to a text-format, useful for backing up your work to GitHub or letting others clone a workspace.
