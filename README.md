# Haskell Basic Calculator(hbc)

## Note

* You must have `cabal` installed on your system

## Build Instruction

* Run `cabal build` to just build the executable
* Run `cabal run` to build and run the executable

## About the Project

We'll be havin 3 data types. We will not be dealing with strings.<br>

* Boolean
* Integers
* Doubles

Supported Operations on Numbers(Ints and Doubles): <br>

* Arithmetic Operations such as `+`,`-`,`/`,`%` and `** (Power)`
* Comparision Operations such as `<`,`<=`,`>`,`>=`,`==` and `/=`
* Assignment Operation `=`
* Negation Operation `-`

We plan to add almost every operations and functions in
[GNU bc](https://en.wikipedia.org/wiki/Bc_(programming_language)#POSIX_bc)
that work with the data types we are supporting and on top of that we'll
be adding support for Boolean types with few essential operations defined
for them. The operations that are parsed by our program as of now are :

* `||`   -> Logical OR
* `&&`   -> Logical AND
* `^`   -> Logical XOR
* `~`    -> Logical NOT
* `->`   -> Logical Imply
* `<->`  -> Logical Double Imply


### Current State

As of now, We are only able to parse the input we're
provided from the user and generate the parse tree for it. It has
most of the basic parsing components ready. It can't parse a
function definition as of now but that ability will be added soon.
Parsing Conditionals, Loops, also hasn't been added yet. I have planned
to implement the basic building blocks at first and then move onto
complex stuff so they are on hold until we have the basic interpreter
ready.

### Libraries

* For parsing,
[parsec](https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html)
is being used
* For arbitrary precision calculation and representaion, We will most likely be using
[numbers](https://hackage.haskell.org/package/numbers)
* Other libraries may also be used if needed.

## Essential planned features

- [ ] Arbitrary precision calculation

- [ ] Ability to define functions

- [ ] Loops

- [ ] Conditionals

- [ ] Math functions provided by GNU bc and a few more if possible

Other feature are to be decided as we continue with the project. If
something seems essential and worth adding, we'll be attempting to
put those also. Open to suggestions as well.

## Contributing

> For contributing refer to the [Contributing.md](./Contributing.md)
