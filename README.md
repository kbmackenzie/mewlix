# Meowscript

Meowscript is a cat-themed esotering programming language. It's an imperative, dynamically typed language with obscure cat-themed syntax and features from both OOP and functional programming. It takes lots of inspiration from Lua syntax--with a kitty flair.

I wrote this language entirely in Haskell as my first 'big' Haskell project. It was a really fun learning experience! It made me really realize how beautiful of a language Haskell is.

I think the best way to show an esolang is with an example, so here's a Meowscript snippet that:
1. Defines a function that asks the user for their name and prints it out n times.
2. Calling that function with the number '10'.

```
=^.x.^= greet(n)
    name = listen()
    take (i = 0) and do (paw at i) while (i < n)
        meow("Hello, "..name.."!")
    meow meow
meow meow

greet(10)
```

As is the nature of esolangs, Meowscript is a joke and not designed for practical use. No non-feline creatures should use it (especially those of the canine variety).

Still, I'll write documentation for it, solely because I can't contain the urge to.

## Shelves and Boxes
Meowscript has only two core data structures: **stacks** (nicknamed 'shelves') and **objects/maps** (nicknamed 'boxes'). It does not have arrays/lists: You must make do with knocking items off shelves instead, as a true feline should.

A stack is, as you might expect, a *stack.* You cannot index it, and you can only perform three operations on it:
1. 'knock over', which pops the item at the top of the stack.
2. 'peek', which peeks and returns the value at the top of the stack, without popping.
3. 'push', an infix operator that pushes the item on the left to the top of the stack on the right.

An example: 
```
books = ["How To Cat", "The C Programming Language", "Learn You A Haskell For Great Good"]

-- Knock "How To Cat" off the shelf.
meow(knock over books)
meow(books) 

-- Put "How To Cat" back on the shelf. That's a good book.
"How To Cat" push books
meow(books)
```

As for boxes, I believe they're best explained by example: 

```
-- A house is really just a very big box, after all.
house_cats = ~(  ^.x.^) BOX!! [
    jake: "orange tabby",
    princess: "orange tabby"
]

meow(cats)
meow(cats.jake)
meow(cats.princess)
```

Additionally, Meowscript has the following features:

### Lexical scoping

Variables are always local to the block they're defined in. If you want to mutate a global variable, you must use the 'bold' keyword:
```
cat = "Jake" -- This variable is global.

=^.x.^= local_cat()
    cat = "Princess" -- This is a different, local variable. This function changes nothing.
meow meow

=^.x.^= global_cat()
    mew cat = "Princess" -- This mutates the global variable 'cat'.
meow meow
```

Do note, however, that the 'mew' keyword looks for the closest variable with that name in the stack, from top to bottom. This means if there's already a local variable named 'cat', 'mew' will choose the local variable.

```
cat = "Jake"

=^.x.^= silly_cat()
    cat = "Princess" -- Local variable overshadows global variable 'cat'.
    mew cat = "Bob" -- This does not mutate the global variable!
meow meow
```

## Functions
Functions, nicknamed 'adventures', are first-class objects *and* value types. You can return a value from a function with the 'bring' keyword:

```
=^.x.^= sum(a, b)
    bring a + b
meow meow
```

Meowscript supports higher-order functions. Any function can take a function as argument and return a function as a result.

Additionally, functions can be nested inside other functions, which can be nested inside other functions, and so on, infinitely. Adventures inside adventures are a fun pattern!

Feline lambdas are also a thing. In Meowscript, they're nicknamed 'mini-adventures'.
They can be defined in a single line like normal expressions:
```
sum = ( ^.x.^)> (a, b) => (a + b)
```
**Note:** Meowscript lamdas do not capture variables. Do not use local variables inside lambdas you intend to use outside of that local scope. If you really want to return them, just put everything inside a box.

## Dynamic Identifiers
Any string can be used as a variable name if you prepend the "~~" operator to it, even if the string is filled with Unicode characters. An example: 
```
~~"(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧" = "SPARKLES!!"
meow(~~"(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧") -- prints "SPARKLES!!"
```
The '~~' operator is nicknamed the "yarn" operator in Meowscript.

**Note:** All variables created from the yarn operator are local. Always. They cannot be mew-ed.

## Box-Oriented Feline Programming

Meowscript supports some features of OOP with boxes; namely, boxes can hold methods, and those methods can access the box they're contained in with the 'home' keyword.

An example:
```
house = ~(  ^.x.^) BOX!! [
   cats = [ "Jake", "Princess" ],
   get_cats = ( ^.x.^)> () => (home.cats)
]

meow(house.get_cats()) -- prints '[ "Jake", "Princess" ]'

<(=^.x.^= )~
Lambdas are fun, but defining functions properly and then adding them to
a box is a cleaner way of defining methods.
Let's define a function to add a cat to our list, and then add it as a
method in our box:
~( =^.x.^=)>

=^.x.^= add_cat(cat)
    cat push home.cats
meow meow 

-- Put that function inside of our box, and call it:
house.add_cat = add_cat

house.add_cat("Cody")
meow(house.get_cats()) -- prints '[ "Cody", "Jake", "Princess" ]'
```

As all boxes are value types, creating new instances of a box is as easy as just copying it. There's no need to worry about shallow copying. Copying a given box creates a new instance of it.

Creating constructor-like functions to create and return more complex boxes is a good pattern.

```
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

As any proper feline would agree, boxes are very important.

## Functional Meowing
Meowscript functions are value types and first-class objects.
