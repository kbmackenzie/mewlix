<img align="left" width="256" height="256" src="logo.svg" alt="Mewlix logo">

**Mewlix** is a cat-themed [esoteric programming language](https://en.wikipedia.org/wiki/Esoteric_programming_language) that compiles to asynchronous vanilla JavaScript. It comes with templates for making little HTML5 games!

Mewlix's compiler is written in pure Haskell, and is a self-contained binary executable. It can build projects, run them in a simple HTTP server, and package the build output neatly into a `.zip` file for upload in websites like [itch.io](https://itch.io/).

As is tradition for all languages, here's a simple `'hello world'` program in Mewlix:
```mewlix
meow "Hello world!"
```
A simple `greet` program:
```mewlix
=^.x.^= greet()
  mew name = listen "What's your name?"
  meow :3"Hello, [name]!"
~meow

do greet
```
*And* a simple example of how to draw a sprite on the canvas using the `graphic` project mode:
```mewlix
from std.graphic takes load, draw, init

do load <- "my_sprite", "./sprite.jpg"
do init <- =^oxo^= () -> draw("my_sprite")
```

Mewlix is similar to C-family languages, with a few key differences. Instead of arrays or lists, Mewlix has a stack-like, LIFO [persistent data structure](https://en.wikipedia.org/wiki/Persistent_data_structure)—affectionately nicknamed a *'shelf'*. Learn more [here](https://github.com/kbmackenzie/mewlix/wiki/Shelf)!

Additionally, it has:
- Cat-themed syntax!
- String interpolation with [yarn strings](https://github.com/kbmackenzie/mewlix/wiki/Expressions#yarn-strings), `:3"like [this]!`
- Lambda functions, defined like this: `=^oxo^= (a, b) -> a + b`
- Classes, affectionately nicknamed [clowders](https://github.com/kbmackenzie/mewlix/wiki/Clowders)!
- A [function composition](https://github.com/kbmackenzie/mewlix/wiki/Operators#function-composition-) operator (`:>`) and a [function application](https://github.com/kbmackenzie/mewlix/wiki/Operators#function-pipes-) operator (`|>`)
- `nand` and `nor` operators, for pessimistic boolean expressions
- *Feline-oriented programming capabilities!*

## Documentation
All of the documentation for Mewlix can be found in the [wiki](https://github.com/kbmackenzie/mewlix/wiki).

If you have any questions, be sure to read the [FAQ](https://github.com/kbmackenzie/mewlix/wiki/FAQ)!

![Looping animation of a cartoon cat resting on a shelf.](https://github.com/kbmackenzie/mewlix/wiki/imgs/cat-shelf.webp)

## Installation
Mewlix's compiler is a self-contained binary executable.

The installation guide can be found [here](./INSTALL.md).

A guide on how to build Mewlix from source can be found [here](./INSTALL.md#build-from-source).

## Special Thanks
A big special thanks to my friend Jade for helping me stay sane through this project and for letting me talk endlessly about it these past few months. I wouldn't have been able to finish it without you, thank you! ♡
