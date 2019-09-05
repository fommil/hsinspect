# `hsinspect`

Inspect `.hs` files using the ghc api.

The goal is to provide a very lightweight (zero dependency) command line interface over the [`ghc`](http://hackage.haskell.org/package/ghc) api for use by text editors such as [`haskell-tng`](https://gitlab.com/tseenshe/haskell-tng.el).

## Features

- [x] list all imported symbols in scope
- [ ] Hoogle-style search of the project dependency graph
- [ ] source location for symbol
- [ ] documentation for symbol

## Contributing

Bug reports and feature requests are a source of anxiety for me, and encourage an unhealthy customer / supplier relationship between users and contributors.

If you wish to contribute, the best thing to do is to let me know about your copy of this repository and we can take it from there. We may both chose to collaborate in one place.

To run the tests type `./tests.sh`

## Why not HIE?

In [Lessons from 6 Software Rewrites](https://medium.com/@herbcaudill/lessons-from-6-software-rewrite-stories-635e4c8f7c22), the author concludes *avoid rewrites and make incremental improvements instead, unless you want to a) remove functionality or b) take a different approach*.

### Remove Functionality

`hsinspect` has a very small scope, and offers only a fraction of the features of [HIE](https://github.com/haskell/haskell-ide-engine). Remove the features from HIE that are not required would be an epic challenge.

### Different Approach

HIE uses the [LSP](https://langserver.org/) so that there is (in theory, but rarely in practice) no additional work required to support a new text editor.

However, LSP servers come with a large cost: they have a lifecycle that must be managed and the text editor needs to know how to communicate with the server. Persistent servers can become a problem in themselves as they can leak resources. The machinary required to support the LSP protocol and a monolithic featureset means that the compiletime is very long (which must be repeated per ghc version).

`hsinspect` is a leightweight command line tool that compiles very quickly and only requires a `ghc.environment` file at runtime. Each text editor must implement custom support but in reality this is not a lot of work because the featureset is small and focused. `hsinspect` does not provide end-user features such as "completion at point" but instead provides raw semantic information that allows the text editor to calculate an answer.

## Known Upstream Issues

`hsinspect` only works if it has access to the flags, arguments, and `PATH` that
is used by the batch compiler. Obtaining this information is very difficult, see
https://github.com/haskell/cabal/issues/6203. `haskell-tng.el` includes a
workaround for this that is unfortunately very slow.

The following might improve things, which are a general problem for Haskell
tooling authors:

- https://github.com/DanielG/cabal-helper/issues/75
- https://github.com/haskell/cabal/pull/5954
- [`hie-bios`](https://github.com/mpickering/hie-bios) for (optional) stack
  support. This will not be enabled by default because it is a large dependency.
