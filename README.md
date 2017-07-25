# Carrie Programming Language

A general purpose programming language. Not anywhere near done. Named after the Sufjan Stevens album "Carrie and Lowell".

## Getting Started

You must have both python 3 and rust installed and on your system path for the compiler to work.

## How It Works

Compiler is a python script which takes a .cr file and converts it to Rust code, then compiles said Rust code.

## How To Use

1) Place "carrie-compiler.py" in the same directory as your Carrie files.
2) use the command "python3 carrie-compiler filename.cr" where filename is the name of your Carrie file.
3) When the lambda prompt comes up, give the name you want your compiled program to have.
4) Use the new binary file however you need.

## Carrie Language Syntax

### Print Function
  print("Hello World!"); This will print "Hello World!" to the console.
  
### Number Bindings
  You can bind numbers to variables by typing number => letter. (As of right now only single letter bindings are supported).
    Ex: 9 => x;
        17 => a;

## Planned Features

Functions, Classes, data types, loops, etc.

## Built With

* [Rust](https://www.rust-lang.org/en-US/) - The second compiler
* [Python3](https://www.python.org/download/releases/3.0/) - What the compiler is written with
* [Carrie and Lowell] When you need a good cry

## Authors

* **Henning Tonko** - *Main Contributor* - [HenningTonko](https://github.com/HenningTonko)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Thank you Cher
