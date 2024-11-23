# Changelog for Mewlix

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
