<p align="center">
  <a href="#"><img src="https://capsule-render.vercel.app/api?type=rect&color=8B008B&height=100&section=header&text=Haskell%20Basic%20Calculator(hbc)&fontSize=50%&fontColor=ffffff" alt="website title image"></a>
  <h2 align="center"> A mathematical scripting language made using Haskell </h2>
</p>


## Note

* You must have `cabal` installed on your system

## Build Instruction

* Run `cabal build` to just build the executable
* Run `cabal run` to build and run the executable
> after building and running the executable sucessfully, you can perform basic operations

```bash
>>>> print 5 + 5
10
>>>> print 10**2
100
>>>> print sin(0)
0.0
>>>> print cos(0)
1.0
>>>> a = 10
>>>> def foo(a){print a;print 100;}
>>>> foo(10)
10
100
>>>> print a
1.0
```

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
most of the basic parsing components ready.
Parsing Conditionals, Loops haven't been added yet. Functions can be defined
and called. Even recursive functions are supported technically, but there's no way of stopping them
because of lack of conditionals. The function implementation is really not good at the moment but
hopefully will be improved soon.

### Libraries

* For parsing,
[parsec](https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html)
is being used
* For arbitrary precision calculation and representaion, We will most likely be using
[numbers](https://hackage.haskell.org/package/numbers). Arbitrary precision is definitely
on hold, it does not seem feasible as of now.
* Other libraries may also be used if needed.

##  Essential planned features

- [ ] Arbitrary precision calculation

- [X] Ability to define functions

- [ ] Loops

- [ ] Conditionals

- [X] Built in Math Functions 

Other feature are to be decided as we continue with the project. If
something seems essential and worth adding, we'll be attempting to
put those also. Open to suggestions as well.

##  Contributing

- For contributing refer to the [Contributing.md](./Contributing.md)
- Read the [code of conduct](./hbc/CODE_OF_CONDUCT.md)
