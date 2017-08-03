# Carrie Programming Language

This is not an enjoyable language to write in. Take the most ugly weird Pascal code and then put it all into one String 
with made of a different pairs of functions and arguments. Completely unreadable. Not anywhere near done. Named after 
the Sufjan Stevens album "Carrie and Lowell".

## Getting Started

Compiler is not working yet. For the foreseable future after I get the compiler working, you will need to 
write your carrie code in Src.hs as to avoid the IO Monad and all the trouble that comes from the IO String 
type.

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
