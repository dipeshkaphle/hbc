<p align="center">
  <a href="#"><img src="https://capsule-render.vercel.app/api?type=rect&color=8B008B&height=100&section=header&text=Haskell%20Basic%20Calculator(hbc)&fontSize=50%&fontColor=ffffff" alt="website title image"></a>
  <h2 align="center">🧮 Calculator made using Haskell 🧮</h2>
</p>

<p align="center">
<img src="https://img.shields.io/badge/language-Haskell-purple?style=for-the-badge"> 
 </p>

## Note

* You must have `cabal` installed on your system

## Build Instruction

* Run `cabal build` to just build the executable
* Run `cabal run` to build and run the executable
### after building and running the executable sucessfully, you perform basic operations
### e.g - 
#### print 5 + 5
10

#### a=10.0
#### b=5.0
#### print a/b
2.0

## 📌 About the Project

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

## 📝 Essential planned features

- [ ] Arbitrary precision calculation

- [ ] Ability to define functions

- [ ] Loops

- [ ] Conditionals

- [ ] Math functions provided by GNU bc and a few more if possible

Other feature are to be decided as we continue with the project. If
something seems essential and worth adding, we'll be attempting to
put those also. Open to suggestions as well.

## 💥 Contributing

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](http://makeapullrequest.com)
[![Open Source Love svg2](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)](https://github.com/ellerbrock/open-source-badges/)

- For contributing refer to the [Contributing.md](./Contributing.md)
- Read the [code of conduct](./hbc/CODE_OF_CONDUCT.md)
