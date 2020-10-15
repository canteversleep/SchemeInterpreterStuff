# SchemeInterpreterStuff

## General Description:

- class project Scheme interpreter written in Scheme that takes inspiration from the techniques of *Essentials of Programming Languages 3 (EOPL-3)*
- currently includes a parser and an evaluator of some Scheme core forms and extended forms as well as primitive procedures
- environments are represented using an `eopl:datatype`

## Work in Progress:

1. syntax expansion procedure that expands a variety of non-core forms into core forms to increase the scope of the target language
2. simpler and more efficient representation of environments. The two main candidates are:
   - procedural representation, as demonstrated in EOPL-3 Ch. 2.2-2.3
   - list of lists representation that focuses particularly on stack efficiency
3. continuation style passing
4. fun stuff:
   - a universal call by value mechanism
   - **maybe** a dynamic scoping switch *lets go retro*

## Future Stuff for Passive Work:

- effective debugging mechanism
- more readable code for some of the longer procedures, such as the parser
- better documentation and doc strings



