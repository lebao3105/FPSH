## FPSH Language

Currently it's made based on Lox language's guide (CraftingInterpreters).

Not completed, especially scanner logic.

## Notes

* Strings in Pascal are null-termined (except ShortString which is not used with `{$H+}`). [Here.](https://wiki.freepascal.org/Character_and_string_types)

## TODOs

* Add `interactive` compiler definiton

* Enumerate each character of each line of the target string

* Proper error handling

## Implementation concept

* [Doing] Parse input by splitting it into array of LINES, each line contains an array of strings (split by spaces).

* Default data types have their own classes in Pascal, with a bit of Python - Pascal's TObject is here:

1. `.ToString()` will cast the objec into a string, of course;

2. `.value` property holds the value of the object;

3. Some may be derived directly from Pascal types, for example arrays. MAY BE???

4. `.copy(destination)` and/or `.copy()` exists

5. Operators (`.plus()`, `.minus()`, `.star()` and so on)

6. For custom classes (that the user made in the script), create a dictionary for its properties. About functions... idk

* Unmatches will have their own class. But it's only made once, only for left things (left paren, left brace, even operators). The instance will be set as matched when a match is found, but only for the nearest instance based on the position in the line/source string:

```
( ( ) 
0 1 2
```

In VSCode, you will see the first paren red-colored. It does not have a match.

For right things, unless there's an unmatched, an exception will be thrown.

## Language concept

* All variables can access their functions and variables defined in Pascal (except `value` which, uhm, holds the value of the variable)

* `operator[]` accepts more than one argument!

```
x = 'hello world'
x['wOrLd', true]
```

Is the same as:

```
x.contains('wOrLd', true);
```