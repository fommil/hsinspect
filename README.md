# `hsinspect`

Inspect `.hs` files using the ghc api.

The goal is to provide a very lightweight (zero dependency) command line interface over the [`ghc`](http://hackage.haskell.org/package/ghc) api for use by text editors such as [`haskell-tng`](https://gitlab.com/tseenshe/haskell-tng.el).

## Features

- [x] obtain ghc flags
- [x] list imported symbols in scope
- [x] list packages, modules, names and types
- [x] list used and unused packages
- [ ] list all symbol uses
- [ ] type at point / [type holes](https://github.com/ghc-proposals/ghc-proposals/pull/280) ([another way](https://github.com/isovector/dynahaskell/blob/master/src/Bindings.hs#L19))

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

`hsinspect` is a leightweight command line tool (and optional compiler plugin) that compiles very quickly and only requires access to the ghc flags used to compile the package. Each text editor must implement custom support but in reality this is not a lot of work because the featureset is small and focused. `hsinspect` does not provide end-user features such as "completion at point" but instead provides raw semantic information that allows the text editor to calculate an answer.

## Installation

The compiler plugin and command line tool must be installed for every project you plan to inspect.

This can be hidden behind a flag (e.g. for libraries that don't wish to publish a dependency):

```
flag ghcflags
  description: Generate .ghc.flags files during compilation
  manual:      True
  default:     False

if flag(ghcflags)
  build-tool-depends: hsinspect:hsinspect
  build-depends: ghcflags
  ghc-options: -fplugin GhcFlags.Plugin
```

and then enable in `cabal.project.local` with

```
flags: +ghcflags
```

We accept that this is a less-than-ideal way to use `hsinspect` but we also accept that it's a problem that can only be fixed by the build tool. If you'd like to help make it better in please implement https://github.com/haskell/cabal/issues/6307

It is possible to enable the plugin on a per-user basis using `-packagedb` and `-packageid`, however that is left as an exercise for people who know what they are doing.

Alternatively, you can create `.ghc.flags` files manually or using the hacks described in https://github.com/haskell/cabal/issues/6203

### Acknowledgements

Thanks to the ghc authors who have made the compiler internals available through an API.

Special thanks to Rahul Muttinieni who has been my guide to that API.
