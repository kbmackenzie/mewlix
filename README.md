# Meowscript

Meowscript is a cat-themed esoteric programming language. It's an imperative, dynamically typed, interpreted language with impractical cat-themed syntax and support for a subset of features from OOP and FP.

It takes a lot of inspiration from the Lua language—just with a lot more cats. \_(:3」∠)\_

I wrote this language entirely in Haskell as my first 'big' Haskell project. It was a really fun learning experience! <3 <3 <3 It made me really realize how beautiful of a language Haskell is!! I'm addicted now!! This project also uses Megaparsec for the lexing/parsing. It's an amazing library!

I think the best way to showcase an esolang is with an example, so here's a Meowscript snippet that:
1. Defines a function that asks the user for their name and prints a greeting n times.
2. Calls that function passing the number '10' as argument.

```lua
=^.x.^= greet(n)
    meow("Meow meow meow?")
    mew name = listen()
    take (mew i = 0) and do (paw at i) while (i < n)
        meow("Meow meow, "..name.."!")
    meow meow
meow meow

greet(10)
```

As is the nature of esoteric languages, Meowscript is a joke language and not designed for practical use.

It's also still under development, so no releases yet. \_(:3」∠)\_

Still, I'll write documentation for it, solely because I can't contain the urge to.

## Basics
A Meowscript file's name should end in '.meows' for organization. \_(:3」∠)\_

Meowscript is dynamically typed and indentation-sensitive! As for the syntax:

### Syntax

String literals:
```lua
"example"
```

Integers:
```lua
10
```

Floatint-point numbers:
```lua
10.0
```

And other primitives:
| Value | Meaning |
|-------|---------|
| happy | true    |
| sad   | false   |
| lonely| null/nil|

Comments:
```lua
-- This is a line comment.
-- Just like in Haskell and Lua!

~( ^.x.^)>
This is a block comment.
Yes, this whole thing. _(:3」∠)_
<(^.x.^ )~
```

Functions:
```lua
=^.x.^= sum (a, b)
    bring a + b
meow meow
```

Control flow:
```lua
mew? (...)
    -- if block _(:3」∠)_
hiss!
    -- else block _(:3」∠)_
meow meow
```

While loops:
```lua
meowmeow (...)
    -- _(:3」∠)_
meow meow
```

For loops:
```lua
take (...) and do (...) while (...)
    -- _(:3」∠)_
meow meow
```

To Do:
1. Write an operator table.
2. Explain the built-in functions.
3. Add a REPL and put this somewhere with some syntax highlighting (?).

### Shelves and Boxes
Meowscript has only two core data structures: **stacks** (nicknamed 'shelves') and **objects/maps** (nicknamed 'boxes'). It does not have arrays/lists: One must make do with knocking items off shelves instead, as a cat should.

A shelf is, as mentioned, a *stack.* You cannot index it, and you can only perform three operations on it:
1. 'knock over', which pops the item at the top of the stack.
2. 'peek', which peeks and returns the value at the top of the stack, without popping.
3. 'push', an infix operator that pushes the item on the left to the top of the stack on the right.

An example: 
```lua
books = ["How To Cat", "All About Servals", "Learn You A Haskell For Great Good"]

-- Knock "How To Cat" off the shelf.
knock over books
meow(books)

-- Put "How To Cat" back on the shelf. That's a good book.
"How To Cat" push books
meow(books)
```

The base library contains a few internal functions that make working with stacks a little easier; namely `sort` and `reverse`. They're implemented internally and are faster than a normal Meowscript function, so don't be afraid of using them! \_(:3」∠)\_

The standard library *(currently in progress)* defines a lot of functions for working with stacks: `map`, `filter`, `fold`, `take_n`, `drop_n`, among others.

The standard library also adds an `index()` function, but you shouldn't use it for two reasons:
1. It's O(n), merely traversing the stack.
2. Knocking items off shelves is far more fun!

As for boxes, I believe they're best explained by example: 

```lua
-- A house is really just a very big box, after all.
house_cats = ~( ^.x.^) BOX!! [
    jake: "tabby",
    princess: "tabby"
]

meow(cats)
meow(cats.jake)
meow(cats.princess)
```

Boxes can contain methods, too, and a cat can use them for OOP. I'll go into detail on this further below.

### Value Types
All types in Meowscript are value types—*except* boxes. They're the only exception.

### Lexical scoping
Meowscript supports lexical scoping. A variable is always local to the block it was first defined in. If a variable already exists in the global scope, however, it can (and will) be mutated inside functions.

To ensure a variable will always be local and will always overshadow any variable with the same name in the outer scope, a cat can use the 'mew' keyword. It's very similar to the 'local' keyword in Lua!

```lua
cat = "Jake" -- This variable is global.

=^.x.^= local_cat()
    mew cat = "Princess" -- This is a different, local variable.
meow meow

=^.x.^= global_cat()
    cat = "Princess" -- This mutates the global variable 'cat'.
meow meow
```

### Functions
Functions, nicknamed 'adventures', are first-class citizens *and* value types. You can return a value from a function with the 'bring' keyword:

```lua
=^.x.^= sum(a, b)
    bring a + b
meow meow
```
Meowscript supports higher-order functions. Any function can take a function as argument and return a function as a result.

Additionally, functions can be nested inside other functions, which can be nested inside other functions, and so on, infinitely. Adventures inside adventures are a fun pattern!

Meowscript also has lambdas! They can be defined in a single line like normal expressions:
```lua
sum = ( ^.x.^)> (a, b) => (a + b)
```
Meowscript lambdas support closures, and will happily capture any local variables a cat uses inside them, so a cat should feel free to use as many as they want.


### Dynamic Identifiers
Any string can be used as a variable name if a cat prepends the "~~" operator to it, even if the string is filled with Unicode characters. An example: 
```lua
~~"(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧" = "SPARKLES!!"
meow(~~"(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧") -- prints "SPARKLES!!"
```
The '~~' operator is nicknamed the "yarn" operator in Meowscript.

**Note:** All variables created from the yarn operator are local (implicitly declared with 'mew').


## Box-Oriented Feline Programming

Meowscript supports some features of OOP with boxes; namely, boxes can hold methods, and those methods can access the box they're contained in with the 'home' keyword.

An example:
```lua
-- A house is just a really big box.
house = ~( ^.x.^) BOX!! [
   cats = [ "Jake", "Princess" ],
   get_cats = ( ^.x.^)> () => (home.cats)
]

meow( house.get_cats() )
-- prints '[ "Jake", "Princess" ]'
```

Lambdas are fun, but defining functions properly and then adding them to a box is a cleaner way of defining methods:

```lua
-- Define a function for our box.
=^.x.^= add_cat(cat)
    cat push home.cats
meow meow 

-- Put that function inside of our box, and call it:
house.add_cat = add_cat

house.add_cat("Cody")
meow( house.get_cats() ) -- prints '[ "Cody", "Jake", "Princess" ]'
```

**Note:** Boxes are reference types. They're Meowscript's only reference type. Thus, when copying them, a cat should be wary of shallow-copying.

Creating constructor-like functions to create and return more complex boxes is a good pattern.

```lua
=^.x.^= new_cat (name, coat, class)
    mew cat = ~( ^.x.^) BOX!! [
        name: name,
        coat: coat,
        class: class,
        get_name:  ( ^.x.^)> () => (home.name),
        get_coat:  ( ^.x.^)> () => (home.coat),
        get_class: ( ^.x.^)> () => (home.class)
    ]
    bring cat
meow meow

bob = new_cat("Bob", "Tabby", "Wizard")
meow(bob)
```

Yay!

## Functional Meowing
Meowscript functions are value types and first-class citizens.

Lambdas are also very lightweight in Meowscript: They're stored the exact same way normal functions are. A cat shouldn't be afraid of using many lambdas: they're fun!

Additionally, stacks are value types in Meowscript, so performing operations on them is always safe: There's no need to worry about references and potentially modifying another stack by accident.

The standard library defines a variety of higher order functions, including (but not limited to): `map`, `fold`, `filter`, `take_while`, `drop_while`, among others.

Additionally, the standard library defines a helper function for partial function application, `ap`, along with a few variations of it. They can be used to partially apply functions, like in this example:

```lua
takes "std.meows"

mew is_three = ap(equal, 3)
is_three(2) -- -> sad
is_three(3) -- -> happy

mew add_two = ap(add, 2)
add_two(1) -- -> 3
add_two(2) -- -> 4
```

## Importing Meows
Meowscript has an import system. You can import `.meows` files either anonymously or with a name:

```lua
-- Importing anonymously. All of the contents of this file are dumped into the current environment.
takes "example.meows"

-- Importing with a name. The contents of this file are stored in an object with the chosen name.
takes "example.meows" as example

```

## Private Keys
Meowscript has limited support for private variables/functions in a yarn ball: When importing a yarn ball, any key starting with an underscore is interpreted as private and isn't imported.

## Notes
[ To Do ]
