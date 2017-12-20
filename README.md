# Carrie Programming Language
Carrie is a small programming language written in Haskell using the Parsec library. There is limited functionality as of V1.0 but more features will be added as time goes on.

## Features
### Parser:
* Parses Functions, the code in the functions and comments wherever
* Parses lines seperated by ';'
* Parses If, While Stmt's
* Parses different kind of lines (assigning and returning)

### Compiler:
* To run the compiler simply type "./CarrieCompiler name.car", where name is the name of your Carrie program.

### Examples

#### ___Assignment___
You assign a value to a variable like this:
```
let x := $1;
```

#### ___Functions___
```
func main(Nothing:x) -> Nothing {
let x := 2 + 0;
}
```

This function calculates the factorial of a number.
```
func fact(Int:x) -> Int {
if (x == 1) START
return 1;
END
let y := x - 1;
DEC(z, Int);
bind(fact)(y)(z);
let f := z * x;
return f;
}
```

#### ___If Stmt's (While Loops work the same pretty much)___
```
if (True) START
let y := 9 + 0;
END
```

You have to test for equality in some when using variables and If Statements
This works:

```
if (z == True) START
...code...
END
```

This does not work:
```
if (z) START
...code...
END
```

#### ___Dec___
DEC is used to declare a variable with a type instead of an initial value. Often used in conjunction with bind.
```
DEC(z, Int);
bind(factorial, 6, z);
```

#### ___Bind___
bind is used to assign the output of a function and its input to a variable. Often used in conjunction with DEC. The following code binds the output of giving factorial the number six and stores it in z. (z -> 720)
```
DEC(z, Int);
bind(factorial, 6, z);
```

#### ___Comments___
They must be on their own line and start and end with a "#"
```
# This is a valid comment #

#This is not#
```

#### ___Lists___
Lists are created by place a list of values seperated by a comma and a space in between two '|' characters
```
let x := |1, 2, 3|;
```

## Built With

* [Haskell](https://www.haskell.org) - Compiler code
* [Parsec](https://hackage.haskell.org/package/parsec) - Only dependancy
* [Carrie and Lowell] When you need a good cry

## Authors

* **Henning Tonko** - *Main Contributor* - [HenningTonko](https://github.com/HenningTonko)

## License

This project is licensed under UnLicense - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Thank you Cher
