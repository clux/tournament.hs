#Tournament.hs ![travis build status](https://secure.travis-ci.org/clux/tournament.hs.png)

Tournament is a pure library which generates tournaments shells, groups
and round robin schedules of various forms using a variety of different rulesets.

## Usage
TODO: generate haddock output to link to in this section

## Installation
TODO: push a version to hackage
````bash
$ cabal-dev install ???
````

## Running tests
Install development dependencies. Note on older versions of cabal:
you may have to install the Test-Suit dependencies manually with `cabal-dev install depX`.

````bash
$ cabal-dev update && cabal-install --enable-tests
````

Run the tests

````bash
$ cabal-dev configure --enable-tests && cabal-dev build && cabal-dev test
````

## License
GPL3-Licensed. See LICENSE file for details.
