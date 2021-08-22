# boros

<img src="Assets/Ouroboros.png" width="150" height="150" />

`boros` (Shortened from [Ouroboros](https://en.wikipedia.org/wiki/Ouroboros)) is a functional programming language that supports `first-class comments`.

## Build instructions

Prerequisites:

* GHC >=8.10.5
* cabal >=3.4.0.0

(Can be installed via [ghcup](https://www.haskell.org/ghcup/))

### Build the interpreter

```sh
cabal new-build
```

### Run the interpreter

```sh
cabal new-run . -- <file.brs> <args for boros script>...
```

e.g.

```sh
cabal new-run . -- Examples/Factorial.brs 100
```

## VS Code Extension

This repo also includes a VS Code extension for Boros syntax highlighting.

You can install it by running the `install-vscode-ext.sh` script.

## Language features

Boros is a strict, impure, dynamically-typed functional language with ML-inspired syntax.

### Syntax

The entire `boros` script file is one big expression, so there's no need for a `main ()` function.

Variables and functions are declared with `let`, and multiple mutually-recursive functions can be declared with `let ... and`.

Here's a simple factorial program:

```ocaml
let fact x =
  if not x then
    1
  else
    x * fact (x - 1)
in

let n = 10 in
fact n
```

Values in `boros` are immutable, with the exception of *lists* and *records*.

Here's how to define and use them:

```haskell
let l = [1, 2, 3] in

l.[0] <- "abc";
print l.[0];

let r = { x = 2, y = 3 } in

r.x <- r.x + 1;
r.z <- 4; {- "You can even add new fields to an existing record!"; -}
```

### First-Class Comments

`boros`  comments are delimited by `{-` and `-}`. Currently, there's no support for line comments or nested comments.

In `boros`, comments *are interpreted as part of the source code*.

Furthermore, `boros` scripts have access to a value called `comments`, which contains the list of all comments in the source file.

The script can modify this list, and when the script finishes, the comments *are replaced with the new values from the `comments` list*, and *the script is re-run*. This continues until either the script stops modifying the comments, or calls the `halt ()` function.

A good example of this can be found in the [Fibonacci](Examples/Fibonacci.brs) example.

### Examples

You can find code examples [here](Examples).
The most interesting ones are the multiple versions of the [Brainfuck interpreter](Examples/BF).
