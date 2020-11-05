# SchemeInterpreterStuff

## General Description:

- class project Scheme interpreter written in Scheme that takes inspiration from the techniques of *Essentials of Programming Languages 3 (EOPL-3)*
- currently includes a parser and an evaluator of some Scheme core forms and extended forms as well as primitive procedures
- environments are implemented using lexical addressing. this is really efficient and removes the need of using variable names altogether and env loojups are fast
- syntax expansion procedure that expands a variety of non-core forms into core forms to increase the scope of the target language
- lexer

## Work in Progress:

1. continuation style passing
2. fun stuff:
   - a universal call by value mechanism
   - **maybe** a dynamic scoping switch *lets go retro*

## Future Stuff for Passive Work:

- effective debugging mechanism
- more readable code for some of the longer procedures, such as the parser
- better documentation and doc strings




