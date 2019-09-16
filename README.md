# My C Language Compiler

This compiler takes a c file as input and generate llvm IR code to be used in JIT or converted to object files to be linked with.
The parser is a hand writing ll(1) parser, and depends on the famous  BNF wrote by Brian W. Kernighan and Dennis M. Ritchie,Prentice Hall, 1988 do not support variable length array

The semantics follows *part* or C99 standard.

Takes llvm as backend.

*notice* grammers which shows in C99 but not in BNF is not implemented

Obviously not implemented (yet) features: Bit-fields, variable lengh array, identifier parameter list, declaration for loop.

## What is this compiler used for

This compiler intents to bring llvm beginners in, and makes a full picture of how a real language compiler looks like, not toy examples on the internet. Write some code and put break points to see how a statement turns to IR code from pure strings.

## Why can't I use #include?

Because I don't have time to implement all key words in C99 standard, mostly header will have these key words. just use `extern function_declaration` instead.

And as mentioned above, this compiler is used for beginners, who is focusing on compiler implementation, so using `extern` is acceptable.

## This compiler is still in debuging

This project is written in my spare time, so it takes amost half a year and still get tons of bugs, including and especialy wrong 
diagnosities. Try to fix these if you see(usually not very hard to debug) will also make you get deeper to this compiler. 
