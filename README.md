# lambda-calc

A very simple λ-calculus interpreter with support for
macros. Mainly just to sanity check my COMP4630 assignment
and to procrastinate from doing actual work. It's probably
broken somewhere, but you can pass it args (either as λ-expressions
or as numerals for church numerals (but you can't mix them))

# Installation

It's just a stack project, so `stack build` should probably work.

# Usage

Read the insufficient documentation with

`stack exec lci -- --help`

For an example 

`stack exec lci -- div.lc --innum --outnum --args="103 7"`
