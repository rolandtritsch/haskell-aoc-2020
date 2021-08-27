# [Advent of Code](https://adventofcode.com) - 2020 (Haskell Edition)

![GHA](https://github.com/rolandtritsch/haskell-aoc-2020/actions/workflows/ci.yml/badge.svg)
[![CircleCI](https://circleci.com/gh/rolandtritsch/haskell-aoc-2020.svg?style=svg)](https://circleci.com/gh/rolandtritsch/haskell-aoc-2020)

![AoC](https://github.com/rolandtritsch/haskell-aoc-2020/blob/trunk/images/aoc-day25.png?raw=true)

To make this work you need to ...

* install [stack](https://www.haskellstack.org)
* run `stack test`
* run `stack build`
* run `stack exec haskell-aoc-exe`
* run `stack test haskell-aoc:haskell-aoc-test-day00 --file-watch` to test a specific day

You can (re)generate the [documentation](http://tedn.life/haskell-aoc-2020) (with some explanations on the solutions) on the `gh-pages` branch (after rebasing it onto `trunk`) with `stack exec -- haddock src/*.hs --html --odir ./doc`.
