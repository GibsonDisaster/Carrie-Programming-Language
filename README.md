# Carrie Programming Language
Carrie is a small programming language written in Haskell using the Parsec library. VERY INCOMPLETE AND NON FUNCTIONAL. Currently it is more of a parser than a compiler.

## Features
### Parser:
* Parses Functions
* Parses lines seperated by ';'
* Starting to be able to parse If statements, etc

### Compiler:
* Nope

## Planned Features
### Parser:
* Parsing While loops, for else loops, etc
* Parsing different kinds of statements (==, >, <, :=)
* Parsing function calls

### Compiler:
* Compile to one of the following (currently undecided):
 1) Rust
 2) C
 3) Go
 4) Java

### Examples
#### Functions
func main(Nothing:x) -> Nothing {
  let x := 2;
}

#### If Stmt's
if (True) {
  let y := 9;
}

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
