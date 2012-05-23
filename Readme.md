#Tournament.hs ![travis build status](https://secure.travis-ci.org/clux/tournament.hs.png)

Tournament is a pure library for generating tournanaments and handling match scoring and
propagation of winners/losers internally.
It is currently in heavy development, see the first
[stable development version](http://hackage.haskell.org/packages/archive/tournament/0.0.1/)
on hackage.

## Features

- `Double | Single` elimination style `Duel` tournaments
- Intelligent `FFA` elimination tournament for experimental setups
- Round robin scheduling
- Group creation
- Encapsulated scoring system.

## Installation
Install the Haskell platform, then

````bash
$ cabal-dev install tournament
````

## Running tests
Install development dependencies. Note on older versions of cabal:
you may have to install the Test-Suite dependencies manually with `cabal-dev install depName`.

````bash
$ cabal-dev update && cabal-dev install --enable-tests
````

Run the tests

````bash
$ cabal-dev configure --enable-tests && cabal-dev build && cabal-dev test
````

## License
GPL3-Licensed for now. See LICENSE file for details.
