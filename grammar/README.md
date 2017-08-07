### Grammar

This Project is used to design Yatol Grammar.

#### Prerequisites

A recent [D compiler](https://dlang.org/download.html#dmd) must be setup.

#### Build and run

- Formal grammar is made in a PEG, in `formal/peg.txt`.
- to auto generate the parser `dub run --config=generate`.
- to parse a sample and test the grammar: `dub run --config=test`.

The last command opens an html file that displays the AST.