# My C Language Compiler

This compiler takes a C file as input and generates LLVM IR codes which to be used in JIT or converted to object files to be linked with.
The parser is a hand writing ll(1) parser, and depends on the famous BNF wrote by Brian W. Kernighan and Dennis M. Ritchie,Prentice Hall, 1988, except that variable length array is not supported

The semantics follows *part* of C99 standard.

Takes LLVM as backend.

*notice* grammers which shows in C99 but not in BNF are not implemented

Obviously not implemented (yet) features: Bit-fields, variable lengh array, identifier parameter list, declaration for loop.

## What is this compiler used for

This compiler intents to bring LLVM beginners in, and makes a full picture of how a real language compiler(front-end) looks like, not toy examples on the internet. Write some code and put break points to see how a statement turns to IR code from pure strings.

## Why can't I use #include?

Because I don't have time to implement all key words in C99 standard, mostly header will have these key words. just use `extern function_declaration` instead.

And as mentioned above, this compiler is used for beginners, who is focusing on compiler implementation, so using `extern` is acceptable.

## This compiler is still in debuging

This project is written in my spare time, so it takes amost half a year and still get tons of bugs, including and especialy wrong 
diagnosities. Bug reports are welcomed.

## How to use
`mycc --print-ast --print-ir --execute test.c`

this command will print the abstract syntax tree, print the IR code, and then execute the `int main()` if exist.

## TODO
1. generate object file.
2. use passes for optimaziation
3. debug messages for gdb.
