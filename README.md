### YATOL [![CI Status](https://travis-ci.org/BBasile/yatol.svg)](https://travis-ci.org/BBasile/yatol) [![codecov](https://codecov.io/gh/BBasile/yatol/branch/master/graph/badge.svg)](https://codecov.io/gh/BBasile/yatol)

YATOL is _Yet Another TOy Langage_, a programming language and its compiler.

Its grammar is formally designed with a PEG and then the parser is written in D.
For now the lexical aspects are still being worked.
A particular focus is put on testing, using built-in D unit tests and coverage.

The language has a C syntax but with a strict LL(1) grammar.
Actually for now there are very few cases of lookups, making it _almost LL(0)_.

On the short term, transpilation is targeted, likely in Object Pascal with the FreePascal compiler (ObjFPC).
