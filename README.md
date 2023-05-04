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
    Jake = "orange tabby"
    Princess = "orange tabby"
]

meow(cats)
meow(cats.Jake)
meow(cats.Princess)
```

Additionally, Meowscript has the following features:
