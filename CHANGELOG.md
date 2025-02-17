# `mewlix` - Changelog

## 1.5.0

- Transpile **yarn strings** more efficiently! 🐱💖
- Fix transpilation for `in` operator.
- Add small optimizations around string coercion.
- Improve how parser handles linebreaks (support CRLF).
- Add better I/O support for environments that don't use UTF-8.
- Standard library changes:
    - Base library:
        - Add bitwise operation functions to `std`.
    - Improve clowders! 🐱
    - Fix innaccurate error messages.

## 1.4.0

- Transpile boolean expressions more efficiently. 🐱💖
- When building in **release mode**, do not compile assertions.
- When parsing multiline strings, ignore the first character if it's a newline.
- Command-line interface changes:
    - Add `--release` build option.
    - Add `--config` option for specifying a path to your config file.
    - Add better help messages.
    - Improve log messages in general.
- Standard library changes:
    - Base library:
        - Changed `std.repeat` function: it now returns a shelf with the collected values.
        - Added `std.sequence` function.
        - Fixed `std.all` function.
        - Fixed how *"nothing"* values are handled by `type of`.
        - Fixed serialization for boxes.
    - Change template structure to make language extensions simpler to implement.
    - Add 'blank' template.
    - Graphic template:
        - Added better loading screen (+ with progress bar!).
        - Fixed broken methods in the `Vector2` clowder.
        - Added **text assets**, manageable through the `graphic.load_text` and `graphic.get_text` functions.
        - Fixed conversion from `Color` clowder to hexcode.
        - Removed `GridSlot` clowder (reason: largely unused).

## 1.3.1

- Compiler improvements:
    - Always export cat trees from a yarn ball.
    - Always coerce box keys to string in lookup expressions (`[]`).
- Parser improvements:
    - Let `or when` statements be chained.
    - Improve parsing of lvalues for assignment. 🐱💖
    - Improve parsing of clowder instantiation expressions (`new <clowder>`).

## 1.3.0

- Command-line interface:
    - Fix silly issue with the `--no-browser` flag not being respected. 🐱💖
    - Add support for the `--quiet` flag for all commands.
- Standard library changes:
    - Let `PixelCanvas` methods accept strings as color values.

## 1.2.0

- Standard library changes:
    - Add [`std.find`](https://kbmackenzie.xyz/projects/mewlix/std#std-find) function.
    - Assorted bug fixes in a few standard library functions.

## 1.1.2

- Assorted bug fixes in the `graphic` project template.

## 1.1.1

- When reporting an error, exit with an appropriate non-zero exit code. 🐱💖

## 1.1.0

- Parser improvements + general clean-up! 🐱💖
- Config file changes:
    - Add `source-files` field, accepting POSIX-like glob patterns for matching source files.
    - Make `assets` field accept POSIX-like glob patterns for matching asset files.
    - Remove `sources` field.
- Command-line interface changes:
    - Replace `mewlix new` command with `mewlix init`.

## 1.0.0

- Initial release.
