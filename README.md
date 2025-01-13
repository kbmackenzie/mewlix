<img align="left" width="256" height="256" src="logo.svg" alt="Mewlix logo">

**Mewlix** is a cat-themed [esoteric programming language][5] that compiles to vanilla JavaScript. It's designed for making little HTML5 games!

Mewlix's compiler is written in pure Haskell, and is a self-contained binary executable. It can build projects, run them in a simple HTTP server, and package the build output neatly into a `.zip` file for upload in websites like [itch.io][6].

As is tradition for all languages, here's a simple `'hello world'` program in Mewlix:

```mewlix
meow "Hello world!"
```

*And* here's how to draw a sprite to the canvas in `graphic` mode:

```mewlix
from std.graphic takes load, draw, init

🐱 draw_cat()
  do draw <- "cat", 0, 0
~meow

do load <- "cat", "path/to/cat.png"
do init <- draw_cat
```

It has...
 
- Cat-themed syntax!
- A stack-like, LIFO [persistent data structure][7]—affectionately nicknamed a *'shelf'*.
- String interpolation—`:3"like [this]!"`—affectionately nicknamed *'yarn strings'*.
- Lambda functions, defined like this: `🐈 (a, b) -> a + b`
- Classes, affectionately nicknamed *'clowders'*.
- A function composition operator (`:>`) and a function application operator (`|>`).
- Feline-oriented programming capabilities.

## Documentation

All of the documentation for Mewlix can be found on [the website][1].

![Looping animation of a cartoon cat resting on a shelf.](https://github.com/kbmackenzie/mewlix/wiki/imgs/cat-shelf.webp)

## Installation

Mewlix's compiler is a **self-contained binary executable**.

The installation guide can be found [here](./INSTALL.md).

A guide on how to build Mewlix from source can be found [here](./INSTALL.md#build-from-source).

## Examples

An assortment of little example projects written in Mewlix can be found [here][4]!

<p float="left">
  <a href="https://github.com/kbmackenzie/mewlix-examples/tree/main/08%20-%20snake%20game">
    <img width="300" height="300" src="https://github.com/kbmackenzie/mewlix/wiki/previews/mewlix-snake.webp" alt="Snake game example.">
  </a>
  <a href="https://github.com/kbmackenzie/mewlix-examples/tree/main/09%20-%20animation%20%2B%20ui">
    <img width="300" height="300" src="https://github.com/kbmackenzie/mewlix/wiki/previews/mewlix-anim.webp" alt="Animation + UI example.">
  </a>
  <a href="https://github.com/kbmackenzie/mewlix-examples/tree/main/03%20-%20animation">
    <img width="300" height="300" src="https://github.com/kbmackenzie/mewlix/wiki/previews/mewlix-balloon.webp" alt="Animation example.">
  </a>
  <a href="https://github.com/kbmackenzie/mewlix-examples/tree/main/05%20-%20playing%20sound">
    <img width="300" height="300" src="https://github.com/kbmackenzie/mewlix/wiki/previews/mewlix-button.webp" alt="Button example.">
  </a>
</p>


## Related Links

All the source code for Mewlix's **base library and templates** can be found [here][3].

**Example projects** can be found [here][4].

### Vim Plugin

A plugin that provides syntax highlighting for `.mews` files can be found [here][8].

## Special Thanks

A big special thanks to my friend Jade for helping me stay sane through this project and for letting me talk endlessly about it these past few months. I wouldn't have been able to finish it without you, thank you! ♡

[1]: https://kbmackenzie.xyz/projects/mewlix
[2]: https://kbmackenzie.xyz/projects/mewlix/faq
[3]: https://github.com/kbmackenzie/mewlix-base
[4]: https://github.com/kbmackenzie/mewlix-examples
[5]: https://en.wikipedia.org/wiki/Esoteric_programming_language
[6]: https://itch.io/
[7]: https://en.wikipedia.org/wiki/Persistent_data_structure
[8]: https://github.com/kbmackenzie/mewlix.vim
