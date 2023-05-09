# Meowscript

Meowscript is a cat-themed esotering programming language. It's an imperative, dynamically typed language with obscure cat-themed syntax and support for a subset of features from OOP and FP. It takes a lot of inspiration from the Lua language, but with a lot more cats.

I wrote this language entirely in Haskell as my first 'big' Haskell project. It was a really fun learning experience! <3 It made me really realize how beautiful of a language Haskell is!! I'm addicted now!!

I think the best way to show an esolang is with an example, so here's a Meowscript snippet that:
1. Defines a function that asks the user for their name and prints it out n times.
2. Calling that function with the number '10'.

```lua
=^.x.^= greet(n)
    mew name = listen()
    take (mew i = 0) and do (paw at i) while (i < n)
        meow("Hello, "..name.."!")
    meow meow
meow meow

greet(10)
```

As is the nature of esolangs, Meowscript is a joke language and not designed for practical use.

Still, I'll write documentation for it, solely because I can't contain the urge to.

## Syntax

Primitives:
| Value | Meaning |
|-------|---------|
| yummy | true    |
| icky  | false   |
| lonely| null/nil|

Comments:
```lua
-- This is a line comment.
-- Just like in Haskell and Lua!

<(=^.x.^= )~
This is a block comment.
Yes, this whole thing.
~( =^.x.^=)>
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
    -- if block
hiss!
    -- else block
meow meow
```

While loops:
```lua
meowmeowmeow (...)
    -- ...
meow meow
```

For loops:
```lua
take (...) and do (...) while (...)
    -- ...
meow meow
```

## Shelves and Boxes
Meowscript has only two core data structures: **stacks** (nicknamed 'shelves') and **objects/maps** (nicknamed 'boxes'). It does not have arrays/lists: One must make do with knocking items off shelves instead, as a cat should.

A shelf is, as you might expect, a *stack.* You cannot index it, and you can only perform three operations on it:
1. 'knock over', which pops the item at the top of the stack.
2. 'peek', which peeks and returns the value at the top of the stack, without popping.
3. 'push', an infix operator that pushes the item on the left to the top of the stack on the right.

An example: 
```
books = ["How To Cat", "All About Servals", "Learn You A Haskell For Great Good"]

-- Knock "How To Cat" off the shelf.
meow(knock over books)
meow(books) 

-- Put "How To Cat" back on the shelf. That's a good book.
"How To Cat" push books
meow(books)
```

As for boxes, I believe they're best explained by example: 

```lua
-- A house is really just a very big box, after all.
house_cats = ~(  ^.x.^) BOX!! [
    jake: "tabby",
    princess: "tabby"
]

meow(cats)
meow(cats.jake)
meow(cats.princess)
```

Boxes can contain methods, too, and a cat can use them for OOP. I'll go into detail on this further below.

## Value Types
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

## Functions
Functions, nicknamed 'adventures', are first-class citizens *and* value types. You can return a value from a function with the 'bring' keyword:

```
=^.x.^= sum(a, b)
    bring a + b
meow meow
```
Meowscript supports higher-order functions. Any function can take a function as argument and return a function as a result.

Additionally, functions can be nested inside other functions, which can be nested inside other functions, and so on, infinitely. Adventures inside adventures are a fun pattern!

Meowscript also has lambdas! They can be defined in a single line like normal expressions:
```
sum = ( ^.x.^)> (a, b) => (a + b)
```
Meowscript lambdas support closures, and will happily capture any local variables a cat uses inside them, so a cat should feel free to use as many as they want.


## Dynamic Identifiers
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
house = ~(  ^.x.^) BOX!! [
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
=^.x.^= new_cat (name, breed, class)
    cat = ~(  ^.x.^) BOX!! [
        name: name,
        breed: breed,
        class: class,
        get_name:  ( ^.x.^)> () => (home.name),
        get_breed: ( ^.x.^)> () => (home.breed),
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

Lambdas are also very lightweight in Meowscript: They're stored the exact same way normal functions are. Don't be afraid of using many lambdas! They're fun!

Additionally, lists are value types in Meowscript, so performing operations on them is always safe: There's no need to worry about references and potentially modifying another list by accident.

The standard library *(currently in progress)* defined a variety of higher order functions, including (but not limited to): *map, fold, filter, take, drop, take_while, drop_while*, among others.


## Importing Meows
Meowscript has an import system. You can import `.meows` files either anonymously or with a name:

```lua
-- Importing anonymously. All of the contents of this file are dumped into the current environment.
takes "example.meows"

-- Importing with a name. The contents of this file are stored in an object with the chosen name.
takes "example.meows" as example
```


## Notes
[ To Do ]
