# `hsinspect`

Inspect `.hs` files using the ghc api.

The goal is to provide a very lightweight (zero dependency) command line interface over the [`ghc`](http://hackage.haskell.org/package/ghc) api for use by text editors such as [`haskell-tng`](https://gitlab.com/tseenshe/haskell-tng.el).

## Features

- [ ] list all imported symbols in scope (IN PROGRESS)

## Contributing

Bug reports and feature requests are a source of anxiety for maintainers, and encourage an unhealthy customer / supplier relationship between users and contributors.

Instead, and following the [anarchical spirit of Haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf), we encourage discussions and debate around code contributions. Merge requests can be raised by anybody and discussed by anybody, and do not need to be complete. An automated test is the only way to report a bug. If the maintainers are convinced by the technical merit and quality of a proposal, they may accept it.

To run the tests type `./tests.sh`

## Known Upstream Issues

Firstly, `hsinspect` only works if all the dependencies of the file under
inspection have been compiled AND are visible to ghc via the `GHC_ENVIRONMENT`
(or `.ghc.environment.`) files that set up the packagedb. It does not require
that the file under inspection is compilable (except that the following can be
parsed: pragmas, module definition, imports).

`stack` does not create env files, so `hsinspect` cannot be used with `stack`.

Environment files can be created in one of two ways with `cabal-install`:

  1. by default 2.4.x will create `.ghc.environment` files for every run of
     `v2-build`, putting them in the base of the project. This was very
     contentious and 3.x will not generate them by default.

  2. `cabal v2-exec` will create a temporary environment file and make it
     available via `GHC_ENVIRONMENT`. Unfortunately, [the parameters to v2-exec
     must match the exact v2-build
     command](https://github.com/haskell/cabal/issues/6182) which is unworkable
     as a way to lauch `hsinspect`.

There are three additional problems:

  1. if the previous compile failed then the `inplace` package is not included,
     which means the file being inspected will not be able to see dependency
     modules that live in the same package (even if they compiled successfully).

  2. `ghc-options` specified in `.cabal` / `cabal.project` /
     `cabal.project.local` files are not visible.

  3. the produced env file does not include references to non-library
     configurations, which means that executables and tests that depend on other
     modules in the same directory are not visible.

We workaround 1 by manually performing a successful compile,
caching the env file, and pointing to it when invoking `hsinspect`.

We workaround 2 by requiring the user to provide language extensions manually
when invoking `hsinspect`.

We cannot workaround 3.

The following might improve things, which are a general problem for Haskell
tooling authors:

- https://github.com/DanielG/cabal-helper/issues/75
- https://github.com/haskell/cabal/pull/5954
- [`hie-bios`](https://github.com/mpickering/hie-bios) for (optional) stack
  support. This will not be enabled by default because it is a large dependency.

## Plan

- [ ] Hoogle-style search of the project dependency graph
- [ ] jump to definition
- [ ] jump to docs
