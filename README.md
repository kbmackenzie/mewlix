<img align="left" width="256" height="256" src="logo.svg" alt="Mewlix logo">

**Mewlix** is a cat-themed [esoteric programming language](https://en.wikipedia.org/wiki/Esoteric_programming_language) that compiles to vanilla JavaScript. It's designed for making little HTML5 games!

Mewlix's compiler is written in pure Haskell, and is a self-contained binary executable. It can build projects, run them in a simple HTTP server, and package the build output neatly into a `.zip` file for upload in websites like [itch.io](https://itch.io/).

As is tradition for all languages, here's a simple `'hello world'` program in Mewlix:
```mewlix
meow "Hello world!"
```
*And* how to draw a sprite to the canvas in `graphic` mode:
```mewlix
from std.graphic takes load, draw, init

=^.x.^= draw_cat()
  do draw <- "cat", 0, 0
~meow

do load <- "cat", "path/to/cat.png"
do init <- draw_cat
```

It has...
 
- Cat-themed syntax!
- String interpolation with [yarn strings](https://github.com/kbmackenzie/mewlix/wiki/Expressions#yarn-strings), `:3"like [this]!`
- A stack-like, LIFO [persistent data structure](https://en.wikipedia.org/wiki/Persistent_data_structure)—affectionately nicknamed a *'shelf'*.
- Lambda functions, defined like this: `=^oxo^= (a, b) -> a + b`
- Classes, affectionately nicknamed [clowders](https://github.com/kbmackenzie/mewlix/wiki/Clowders)!
- A [function composition](https://github.com/kbmackenzie/mewlix/wiki/Operators#function-composition-) operator (`:>`) and a [function application](https://github.com/kbmackenzie/mewlix/wiki/Operators#function-pipes-) operator (`|>`)!
- `nand` and `nor` operators, for picky boolean expressions.
- Feline-oriented programming capabilities.

## Documentation
All of the documentation for Mewlix can be found in the [wiki](https://github.com/kbmackenzie/mewlix/wiki).

If you have any questions, be sure to read the [FAQ](https://github.com/kbmackenzie/mewlix/wiki/FAQ)!

![Looping animation of a cartoon cat resting on a shelf.](https://github.com/kbmackenzie/mewlix/wiki/imgs/cat-shelf.webp)

## Examples

An assortment of small programs + small games written in Mewlix can be found [here](https://github.com/kbmackenzie/mewlix-examples).

<img width="500" height="500" src="https://github.com/kbmackenzie/mewlix-examples/raw/main/previews/mewlix-snake.webp" alt="Snake game example.">

## Installation
Mewlix's compiler is a self-contained binary executable.

The installation guide can be found [here](./INSTALL.md).

A guide on how to build Mewlix from source can be found [here](./INSTALL.md#build-from-source).

## Special Thanks
A big special thanks to my friend Jade for helping me stay sane through this project and for letting me talk endlessly about it these past few months. I wouldn't have been able to finish it without you, thank you! ♡
