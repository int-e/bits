# bit** programming language

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

<code>ghc --make bit-h</code> or <code>cabal install</code>

## usage

<code>./bit-h [-c] file</code>

* <code>-c</code> enables character-based I/O

## files

* [bit-h.hs](bit-h.hs): interpreter source
* [examples/cat](examples/cat), [examples/rev](examples/rev): cat and reverse
* [examples/hello](examples/hello): Hello, world!
* [examples/rot13](examples/rot13): rot13, generated from [examples/rot13.pp](examples/rot13.pp)
* [examples/rot13-lut](examples/rot13-lut): another rot13, generated from [examples/rot13.pp](examples/rot13-lut.pp)
* [examples/quine](examples/quine): quine
* [examples/brainfuck](examples/brainfuck): Brainfuck interpreter, generated from [examples/brainfuck.pp](examples/brainfuck.pp)
