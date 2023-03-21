# JSON Parser Benchmark (JPB) for Haskell

The files in this project install package dependencies for the JSON
parsing benchmark program. There are 2 executables, each testing one
of two Haskell JSON parsing libraries.

 1. [Aeson](https://hackage.haskell.org/package/aeson ) is written
    mostly in Haskell with a few parts written in C to eek-out the
    best performance possible. To the author's knowledge, this has
    been the most widely-used JSON parser in the Haskell software
    ecosystem for many years now.

 2. [Hermes-JSON](https://hackage.haskell.org/package/hermes-json ) is
    a Haskell wrapper around the C++ library
    [SMIDJSON](https://github.com/simdjson/simdjson ). As the name
    suggests, it should take advantage of the SIMD instructions
    built-in to the CPU hardware of your computer to parse JSON data.

    **WARNING:** this version of the benchmark should not be used
    until various issues with the benchmark have been resolved.

## Defeating Haskell's lazy semantics

### Aeson

For the Aeson version of the test use the `deepseq` function (Reduced
Normal Form) from the Haskell [DeepSeq](https://hackage.haskell.org/package/deepseq )
library to force the parsed JSON data structure to exist in
memory. Without this, the compiler will take advantage of Haskell lazy
semantics to optimize away the unnecessary work of storing the entire
parsed JSON data structure in memory, which it would otherwise do
since this data structure is never used in any way. **DeepSeq** is
usually installed by default with most Haskell installations, and so
is available on most systems.

### Hermes

The latest version of Hermes (at the time of this writing) is 0.5, but
for various reasons a much older and more inefficient version 0.2 must
be used for compatibility with other package dependencies (probably
Aeson). This cancels-out any possible benefit from using the SIMD
instructions of the CPU and introduces a ridiculous amount of
computing overhead. The code is included here with the hope that it
can be tested with a more recent version of the `hermes-json` library
in the near future.

For the Hermes version of the test, we cannot use `deepseq` because
the fully parsed data structure is not exposed to Haskell from the C++
library. Instead you are supposed to lazily (on-demand) traverse the
structure and extract only portions you need, while parser only parses
that which is necessary. In other words the C++ JSON parser is also
lazy.

To defeat lazy semantics, a JSON data structure is defined which
forces the entire structure to be copied from C++ to Haskell. This
could possibly double the amount of memory that would consumed by
simply using the C++ parser alone.

**Note** that this would not be the case in ordinary production
software. A much more efficient Haskell data structure would
ordinarily be used, but this is not possible for this particular
benchmark given that the task is not to query or transform the JSON
data, but simply to buffer it in memory.

### Building the benchmark executables

Build with `cabal` and then install it into the current working
directory:

```sh
# Build both versions of the benchmark
cabal configure
cabal v2-build

# Install the Aeson version of the benchmark program:
cabal v2-install jpb-aeson --installdir=./

# Install the Hermes version of the benchmark program:
cabal v2-install jpb-hermes --installdir=./
```

The above should have created symbolic links to the ELF executable
programs in the current (`./`) directory that were built by GHC :
`jpb-aeson` and `jpb-hermes`. You may then run these executables like
so:

```
time ./jpb-aeson path-to-test-file.json
time ./jpb-hermes path-to-test-file.json
```
