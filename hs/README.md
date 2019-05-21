# bitch programming language

This is a reimplementation of https://github.com/Helen0903/bitch in Haskell,
with the following differences:

* use big integers

* additional operation <code>%</code> dumps the current state and the next
  instruction

  format:
  <code>% .. accumulator (in hex) | reverse storage .. instruction</code>

  example:
  <code>% .. 0000 0000 0000 0001 | 8000 0000 0000 0000 .. ^^[1</code>

## compile

<code>ghc --make bitch</code> or <code>cabal install</code>

## usage

<code>./bitch [-c] file</code>

* <code>-c</code> enables character-based I/O

## files

* [bitch.hs](bitch.hs): interpreter source
* [examples/cat](examples/cat), [examples/rev](examples/rev): cat and reverse
* [examples/hello](examples/hello): Hello, world!
* [examples/rot13](examples/rot13): rot13, generated from [examples/rot13.pp](example/rot13.pp)
* [examples/quine](examples/quite): quine
* [examples/brainfuck](examples/brainfuck): Brainfuck interpreter, generated from [examples/brainfuck.pp](example/brainfuck.pp)
