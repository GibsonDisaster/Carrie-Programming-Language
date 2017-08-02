# Carrie Programming Language

A general purpose programming language. Not anywhere near done. Named after the Sufjan Stevens album "Carrie and Lowell".

## Getting Started

Compiler is not working yet. For the foreseable future after I get the compiler working, you will need to 
write your carrie code in Src.hs as to avoid the IO Monad and all the trouble that comes from the IO String 
type.

### Hello World Carrie

To write a Hello World program in Carrie, first declare a function called main by typing "funcdec main" into
the text function in Src.hs. Then type "print Hello/World" after that. "/" is how you write spaces in Carrie,
the compiler will automatically replace all "/" with spaces. The final piece of code to write is 
"funcend main". This ends the function called main.

The whole program looks like:
"funcdec main print Hello/World funcend main"

### Variables

You can declare a variable name "age" and assign it the value 17 by writing: 
"intdec age assign 17"

## How It Works

Compiler is written in Haskell using only standard libraries and functions written by myself. No external libraries required
to use this compiler.

## Planned Features

Functions, Classes, data types, loops, etc. Carrie will be a mostly Imperative programming language, with Functional
influences.

## Built With

* [Haskell](https://www.haskell.org) - Compiler code
* [Carrie and Lowell] When you need a good cry

## Authors

* **Henning Tonko** - *Main Contributor* - [HenningTonko](https://github.com/HenningTonko)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Thank you Cher
