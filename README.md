# Carrie Programming Language
Carrie is a small programming language written in Haskell using the Parsec library. VERY INCOMPLETE AND NON FUNCTIONAL. Currently it is more of a parser than a compiler.

## Features
### Parser:
* Parses Functions
* Parses lines seperated by ';'
* Parses If, While Stmt's
* Parses different kind of lines (assigning and returning)

### Compiler:
* Nope

## Planned Features
### Parser:
* Parsing While loops, for else loops, etc
* Parsing different kinds of statements (==, >, <, :=)
* Parsing function calls
* Allowing you to indent your code (not allowed yet)

### Compiler:
* Compile to one of the following (currently undecided):
 1) Rust
 2) C
 3) Go
 4) Java

### Examples
#### ___Functions___
func main(Nothing:x) -> Nothing { \
let x := 2; \
}

#### ___If Stmt's (While Loops work the same pretty much)___
if (True) START \
let y := 9; \
END

You have to test for equality in some when using variables and If Statements
This works:

if (z == True) START\
...code...\
END

This does not work:

if (z) START\
...code...\
END

#### ___Comments___
They must be on their own line and start and end with a "#"

\# This is a valid comment \#

#This is not#

## Built With

* [Haskell](https://www.haskell.org) - Compiler code
* [Parsec](https://hackage.haskell.org/package/parsec) - Only dependancy
* [Carrie and Lowell] When you need a good cry

## Authors

* **Henning Tonko** - *Main Contributor* - [HenningTonko](https://github.com/HenningTonko)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Thank you Cher
