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

   stack exec oscoin -- --blockstore blockstore.db --nakamoto-consensus-lenient --block-time-lower 600 --beneficiary 0x0000000000000000000000000000000000000000

Testing
-------
Run the tests with file-watching with::

  stack test --file-watch --fast

To load the tests in the repl, run::

  stack repl oscoin:lib oscoin:test

Then run ``:main`` from the repl to run the tests, and reload with ``:r`` as
necessary. You can run a subset of tests with ``:main -p "test
pattern"``.

Using the ``--crypto`` option allow the tests to be run with either the
mock crypto::

  stack test --ta "--crypto mock"

Or with the "real world" implementation (i.e. the same one used in production)::

  stack test --ta "--crypto realworld"

If no ``--crypto`` option is specified, it defaults to the mock one.

Test organization
~~~~~~~~~~~~~~~~~

All tests are contained in the ``test`` directory. The tests contained
in a module are exported through the ``tests`` value. The ``tests``
export defines a is a ``tasty`` test group with with the same name as
the module name. Tests from test modules are imported and collected in
the ``Main`` module.

For unit tests there is a one-to-one correspondence between the tested
module and the test module. For example the tests for the code in
module ``A.B.C`` are contained in ``Test.A.B.C`` which exports

.. code-block:: haskell

   tests = testGroup "Test.A.B.C" [ someTest ]

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
