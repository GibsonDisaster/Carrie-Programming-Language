# Carrie Programming Language

This is not an enjoyable language to write in. Take the most ugly weird Pascal code and then put it all into one String 
with made of a different pairs of functions and arguments. Completely unreadable. Not anywhere near done. Named after 
the Sufjan Stevens album "Carrie and Lowell".

## Getting Started

To compile Carrie code, place CarrieCompiler in the same directory as a "main.cr" file. Then run the compiler with 
"./CarrieCompiler". The output file will be named "Carrie".

### Syntax
Your code must be made up of "pairs". Where a pair is made by typing "function data". Only spaces are allowed, those are
used to seperate functions and data. Newlines, tabs, etc will break your code DO NOT USE.
Ex: "print Hello/World" 

### Hello World Carrie

To write a Hello World program in Carrie, first declare a function called main by typing "funcdec main()" into
the text function in Src.hs. Then type "print Hello/World" after that. "/" is how you write spaces in Carrie,
the compiler will automatically replace all "/" with spaces. The final piece of code to write is 
"funcend main". This ends the function called main.

The whole program looks like:
"funcdec main() print Hello/World funcend main"

### Variables

You can declare a variable name "age" and assign it the value 17 by writing: 
"intdec age assign 17"

### Bindings and Adding Ints

You can bind the outcome of the addition of two ints by using the bind command with the bindings name followed by 
addint followed by the first number followed by with followed by the second number.

bind binding_name addint num1 with num2

example:

"bind z addint 5 with 100"

z would equal 105

### Commenting Code

You can comment in your code using the CMT key word followed by your comment (With each word seperated by a '/' instead
of a space)

example:

"funcdec main intdec z CMT Just/initialized/z funcend main"

### Declaring A Function That Takes Arguments

#### Zero Argument Function

funcdec main()

#### Single Argument Function

funcdec main(int:n)

#### Multiple Argument Function

funcdec main(int:n+int:m+int:x)

### Calling Functions

Just type "funccall" followed by the function name with each argument seperated by a comma with NO SPACE in between.

Ex:
funccall main()
funccall main(3,4,5)

### For Loops

example for loop: 

"forstart int:i=0 forstop i>10 forfunc i++ forend"

### If Then Else Loops

example if-then-else loop:

"ifstart NULL var 1 eq == var 1 boolexpr then return true boolexpr else return false ifclose NULL"

### Return statements

Used to return a value.

Ex:

"return 1"

## Factorial Function

The following is how you would write a function to find the factorial of a number in Carrie:

"funcdec factorial(int:n) ifstart NULL var n eq == var 1 boolexpr then return 1 boolexpr else return n*factorial(n-1) ifclose NULL funcend factorial"

## How It Works

Compiler is written in Haskell using only standard libraries and functions written by myself. No external 
libraries required to use this compiler. 

## Built With

* [Haskell](https://www.haskell.org) - Compiler code
* [Carrie and Lowell] When you need a good cry

## Authors

* **Henning Tonko** - *Main Contributor* - [HenningTonko](https://github.com/HenningTonko)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Thank you Cher
