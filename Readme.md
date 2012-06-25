# Tournament.hs [![Build Status](https://secure.travis-ci.org/clux/tournament.hs.png)](http://travis-ci.org/clux/tournament.hs)

Tournament is a pure library for generating tournanaments and handling match scoring and
propagation of winners/losers internally. It is currently under development, but a preliminary version
is [available on hackage](http://hackage.haskell.org/package/Tournament).

## Features

- `Double | Single` elimination style `Duel` tournaments
- Intelligent `FFA` elimination tournament for experimental setups
- Round robin scheduling
- Group creation
- Encapsulated scoring system.

## Installation
Install the Haskell platform, then

````bash
$ cabal-dev update && cabal-dev install tournament
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
MIT-Licensed. See LICENSE file for details.
