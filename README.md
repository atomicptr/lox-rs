# lox-rs

My implementation of the lox programing language from the [Crafting Interpreters](https://craftinginterpreters.com/) book.

## Changes

This implementation has some changes different from the books implementation (some of these were part of the extra challenges)

- Dynamic but strongly typed, there are type checks for operations
- Logical operators evaluate to boolean values instead, e.g. "nil or 5" becomes true and not 5
- Better error messages with context
- continue/break statements for loops
- support for anonymous functions

## License

GPLv3
