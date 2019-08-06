# `hsinspect`

Inspect `.hs` files using the ghc api.

The goal is to provide a very lightweight (zero dependency) command line interface over the [`ghc`](http://hackage.haskell.org/package/ghc) api for use by text editors such as [`haskell-tng`](https://gitlab.com/tseenshe/haskell-tng.el).

## Features

- [ ] list all imported symbols in scope (IN PROGRESS)

## Contributing

Bug reports and feature requests are a source of anxiety for maintainers, and encourage an unhealthy customer / supplier relationship between users and contributors.

Instead, and following the [anarchical spirit of Haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf), we encourage discussions and debate around code contributions. Merge requests can be raised by anybody and discussed by anybody, and do not need to be complete. An automated test is the only way to report a bug. If the maintainers are convinced by the technical merit and quality of a proposal, they may accept it.

To run the tests type `./tests.sh`

## Plan

- [ ] Hoogle-style search of the project dependency graph
- [ ] jump to definition
- [ ] jump to docs
