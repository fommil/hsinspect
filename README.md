# `hsinspect`

Inspect `.hs` files using the ghc api.

Provides a very lightweight (zero dependency) command line interface over the [`ghc`](http://hackage.haskell.org/package/ghc) api for use by text editors.

## Features

- [x] obtain ghc flags
- [x] list imported symbols in scope
- [x] list packages, modules, names and types
- [x] list used and unused packages (`-Wunused-packages` before ghc-8.10)

## Contributing

Bug reports and feature requests are a source of anxiety for me, and encourage an unhealthy customer / supplier relationship between users and contributors.

If you wish to contribute, the best thing to do is to let me know about your copy of this repository and we can take it from there. We may both chose to collaborate in one place.

To run the tests type `./tests.sh`

## Why not Haskell Language Server?

In [Lessons from 6 Software Rewrites](https://medium.com/@herbcaudill/lessons-from-6-software-rewrite-stories-635e4c8f7c22), the author concludes *avoid rewrites and make incremental improvements instead, unless you want to a) remove functionality or b) take a different approach*.

### Remove Functionality

`hsinspect` has a very small scope, and offers only a fraction of the features provided by the [Haskell Language Server](https://github.com/haskell/haskell-language-server).

Whereas the Haskell Language Server aims to implement every aspect of the [LSP](https://langserver.org/), `hsinspect` asks "which features do we **need**?" and attempts to do this with minimal dependencies to ensure a fast installation.

### Different Approach

Tools that deal with Haskell source code require access to the flags that are passed to the compiler, listing language extensions, dependencies, error levels, and preprocessor flags.

`hsinspect` obtains the compiler flags via a compiler plugin (`ghcflags`) which dumps the exact flags that are used when compiling the project, at no cost to performance. Compare to [`hie-bios`](https://github.com/mpickering/hie-bios) which attempts to extract the compiler flags from the build tool. The tradeoff is: `hie-bios` is easy to install but is slow and not always correct, whereas `ghcflags` requires intrusive changes to the `.cabal` file and is 100% correct with no performance overhead.

A single LSP server binary, shared by all projects, locks the user in to one version of `ghc`. However, `hsinspect` is compiled per-package uses the exact same version of `ghc` for maximum compatibility.

A direct CLI interface avoids many problems associated with language servers: scalability, bloat, memory leaks, lifecycle management, and networking issues. A direct interface means performance is exceptional, e.g. imported symbols can be stored in-memory in the text editor, avoiding network calls on every keystroke. It is also possible to write Haskell-specific features and caching strategies that go outside the realm of the LSP, e.g. [Emacs prefix arguments](https://www.gnu.org/software/emacs/manual/html_node/emacs/Arguments.html) can change the behaviour of the commands.

That said, `hsinspect-lsp` is provided as a wrapper that implements a very small subset of the LSP, offering basic semantic features to a much wide range of text editors. The `hsinspect-lsp` executable does not need to be recompiled for each version of `ghc` and can be installed system-wide.

## Editor Support

- Emacs [`haskell-tng`](https://gitlab.com/tseenshe/haskell-tng.el) (provides both direct and LSP implementations)
- VSCode [HsInspect Language Server](https://marketplace.visualstudio.com/items?itemName=rahulmutt.hsinspect) (LSP only)

LSP users should install the `hsinspect-lsp` command line tool

```
cd ~ && cabal update && cabal install hsinspect-lsp -w ghc-8.8.3 --overwrite-policy=always
```

(this command will also upgrade the LSP server)

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
