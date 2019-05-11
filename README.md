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

* (fix) input in chained instruction reads from same line as unchained
  instructions

* (fix?) <code>#</code> requires a literal, unlike bitwise operations which
  can be followed by other operations (that's my reading of the description
  at https://esolangs.org/wiki/bitch)

## compile

<code>ghc --make bitch</code>

## usage

<code>./bitch [-c] file</code>

* <code>-c</code> enables character-based I/O

## files

* [bitch.hs](bitch.hs): interpreter source
* [hello](hello): a hello world program
* [rot13](rot13): a rot13 program
* [add](add): 32 bit addition
* [sub](sub): 32 bit subtraction
