<img align="left" width="256" height="256" src="logo.svg">

**Mewlix** is a cat-themed esoteric programming language that compiles to ES6 Javascript. It comes with templates for making interactive console applications and little HTML5 games.

Mewlix's compiler is written in pure Haskell, and is a self-contained binary executable. It can build projects, run them in a simple HTTP server, and package the build output neatly into a `.zip` file for upload in websites like [itch.io](https://itch.io/).

As is tradition for all languages, here's a simple `'hello world'` program in Mewlix:
```mewlix
yarn ball main 

meow "Hello world!"
```
And a simple `greet` program:
```mewlix
yarn ball main

=^.x.^= greet()
  mew name = listen "What's your name?"
  meow "Hello, "..name.."!"
meowmeow
```

Mewlix is very similar to languages like Python and Javascript, with one catch: Instead of arrays or lists, Mewlix has *shelves*: a stack-like, LIFO [persistent data structure](https://en.wikipedia.org/wiki/Persistent_data_structure). To learn more about shelves and how they work, [read the documentation page for shelves](todo)!

In addition, Mewlix has support for higher order functions and anonymous functions, and supports a subset of object-oriented programming with classes—which, in Mewlix, we call [clowders](https://www.merriam-webster.com/dictionary/clowder).

## Documentation
All of the documentation for Mewlix can be found in the [wiki](https://github.com/KBMackenzie/mewlix/wiki).

## Installation
Mewlix's compiler is a self-contained binary executable. It can be run from anywhere, and has no external dependencies.

The installation guide can be found [here](./INSTALL.md).

A guide on how to build Mewlix from source can be found [here](./INSTALL.md#build-from-source).

## Special Thanks
A big special thanks to my friend Jade for helping me stay sane through this project and for letting me talk endlessly about it these past few months. I wouldn't have been able to finish it without you, thank you! ♡

A big special thanks to my friend Atom for letting me rant endlessly every time I found a roadblock in this project. I really appreciate it.
