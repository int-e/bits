# bitch programming language

This repository contains two implementation of
https://github.com/Helen0903/bitch, one in Haskell, and the other in C++.
The following distinguishing features are implemented:

* big integers, offering unbounded memory

* the additional operation <code>%</code> dumps the current state and the
  next instruction

  format:
  <code>% .. accumulator (in hex) | reverse storage .. instruction</code>

  example:
  <code>% .. 0000 0000 0000 0001 | 8000 0000 0000 0000 .. ^^[1</code>

## files

* [hs/bitch.hs](hs/bitch.hs): Haskell interpreter source
* [cc/shifty.cc](cc/shifty.cc): C++ interpreter source
* [examples/cat](examples/cat), [examples/rev](examples/rev): cat and reverse
* [examples/hello](examples/hello): Hello, world!
* [examples/rot13](examples/rot13): rot13
* [examples/quine](examples/quite): quine
* [examples/brainfuck](examples/brainfuck): Brainfuck interpreter, generated from [examples/brainfuck.pp](example/brainfuck.pp)
