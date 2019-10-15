# `hsinspect`

Inspect `.hs` files using the ghc api.

The goal is to provide a very lightweight (zero dependency) command line interface over the [`ghc`](http://hackage.haskell.org/package/ghc) api for use by text editors such as [`haskell-tng`](https://gitlab.com/tseenshe/haskell-tng.el).

## Features

- [x] obtain ghc flags
- [x] list all imported symbols in scope
- [x] list all modules that may be imported
- [x] list all used and unused packages
- [ ] Hoogle-style search of the project dependency graph
- [ ] source location for symbol
- [ ] documentation for symbol
- [ ] calculate packages that are actively used by sources in a folder

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

### Plugin

The compiler plugin must be installed for every project you plan to inspect:

1. add a dependency on `hsinspect`
2. add `-fplugin HsInspect.Plugin` to `ghc-options`

It is possible to enable the plugin on a per-user basis using `-packagedb` and `-packageid`, however that is left as an exercise for people who know what they are doing.

Alternatively, you can create a `.ghc.flags` and `.ghc.version` file manually or using the hacks described in https://github.com/haskell/cabal/issues/6203

### Command Line Tool

You must install `hsinspect` for every version of `ghc` that you plan to use, e.g.

```
rm -f ~/.cabal/bin/hsinspect
for V in 8.4.4 8.6.5 ; do
  cabal v2-install hsinspect -w ghc-$V -O2 &&
  mv -f ~/.cabal/bin/hsinspect ~/.cabal/bin/hsinspect-ghc-$V
done
```

<!--
for V in 8.4.4 8.6.5 ; do
  cabal v2-install exe:hsinspect -w ghc-$V -O2 &&
  mv -f ~/.cabal/bin/hsinspect ~/.cabal/bin/hsinspect-ghc-$V
done
-->
