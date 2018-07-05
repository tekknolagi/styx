### Styx [![CI Status](https://travis-ci.org/BBasile/styx.svg)](https://travis-ci.org/BBasile/styx) [![codecov](https://codecov.io/gh/BBasile/styx/branch/master/graph/badge.svg)](https://codecov.io/gh/BBasile/styx)

Styx programming language and its compiler.

The language has mostly a C syntax and a simple LL(1) grammar.
Its grammar is formally designed using a PEG and the compiler is fully hand written in D.
A particular focus is put on testing, using D features such as in code unit tests or code instrumentation for coverage.

### Status

#### Accomplishment

- lexical: **90%**, would be enough to bootstrap, but needs many improvements.
- semantic: **10%**, started.
- codegen: **0%**, LLVM ? libfirm ? transpilation ?

#### Health

| Compiler          | Status
|-------------------|------------------------------
| DMD & LDC stable  | [![CI Status](https://travis-ci.org/BBasile/styx.svg)](https://travis-ci.org/BBasile/styx)
| DMD nightly       | [![Build Status](https://semaphoreci.com/api/v1/bbasile/styx/branches/master/shields_badge.svg)](https://semaphoreci.com/bbasile/styx)
