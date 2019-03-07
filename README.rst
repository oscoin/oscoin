oscoin
======
Oscoin full node reference implementation in Haskell.

**Warning: this implementation is a work in progress, and not ready for
production use. The source code is provided here for informational purposes
only.**

Building
--------
Build the full node with::

   stack build

Running
-------
First, generate a key pair for your node::

   stack exec oscoin-cli -- keypair generate

Then run the full node with::

   stack exec oscoin -- --no-empty-blocks

Testing
-------
Run the tests with file-watching with::

  stack test --file-watch --fast

To load the tests in the repl, run::

  stack repl --test --main-is oscoin:test:test

Then call the ``main`` function to run them, and reload with ``:r`` as
necessary.

Profiling
---------
To profile the test-suite, run::

  stack test --profile -j4 --work-dir .stack-work-prof

This will output a file, ``test.prof`` which you can convert to an interactive
``svg`` with::

  ghc-prof-flamegraph test.prof

This will require installation of ``ghc-prof-flamegraph``. Then open the ``.svg``
with any browser.

Docs
----
To generate local package documentation, run::

  stack haddock oscoin

To open the docs, run::

  stack haddock oscoin --open

It's also possible to browse documentation/tutorials for individual components
in the `docs <./docs>`_ directory:

- `Telemetry <./docs/telemetry.md>`_ documentation;

These documents are literate Haskell files pre-processed with
`markdown-unlit <https://github.com/sol/markdown-unlit>`_, so that they are
executable code which can be kept up-to-date with the codebase. We also have
a ``test-suite`` which ensure these documentation files keep compiling.

Copyright
---------
Copyright (c) Monadic, 2017-2019