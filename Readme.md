#Tournament.hs ![travis build status](https://secure.travis-ci.org/clux/tournament.hs.png)

Tournament is a pure library whice generates tournanaments deals with scoring of matches and
the propagation of winners internally.
It is currently in heavy development and is not yet on hackage.

## Features

- `Double | Single` elimination style `Duel` tournaments
- Intelligent `FFA` elimination tournament for experimental setups
- Round robin scheduling
- Group creation
- Encapsulated scoring system.

TODO:
- properly encapsulate types, let access be in serializable form only? if so how?
- decide on FFA style stuff
- generate haddock output
- fix tests again [only broken due to exhaustion of cases..]
- release first version

## Usage
link to haddock output here

## Installation
Install the Haskell platform, then

````bash
$ cabal-dev install ???
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
GPL3-Licensed. See LICENSE file for details.
