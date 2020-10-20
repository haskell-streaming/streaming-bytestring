## Unreleased

## 0.2.0 (TBD)

#### Breaking changes [#42]

- Switch names of `fold` and `fold_` in the non-`Char8` modules.  The
  corresponding `Char8` functions and the rest of the library uses `_`
  for the variant that forgets the `r` value.

#### Fixed [#42]

- Fix intersperse implementation to ignore any initial empty chunks.
- Fix intercalate implementation to not insert anything between the
  final substream and the outer stream end.
- Fix bug in `unlines`, it incorrectly differentiates between `Chunk ""
  (Empty r)` and `Empty r`.  There's no such difference.  Also this
  dubious distinction was not made when the two forms are behind a
  monadic effect.
- Import 'SPEC' from GHC.Types, as of 9.0, GHC.Exts no longer exports
  `SpecConstrAnnotation`.

#### Fixed [#43]

- An edge case involving overflow in `readInt`. [#43]

[#42]: https://github.com/haskell-streaming/streaming-bytestring/pull/42
[#43]: https://github.com/haskell-streaming/streaming-bytestring/pull/43

#### Added [#42]

- Add missing `zipWithStream` export in `Char8` module
- Add missing `materialize` and `dematerialize` exports in "Word8"
  module
- Relax signature of `toStrict_` to allow any `r`, not just `()`.

#### Performance [#42]

- Make packChars more efficient by leaving c2w conversion to the
  ByteString library (should be free, or at least much cheaper in
  `poke p (c2w c)`, since we avoid converting the input stream).
- More performant rewrite of `denull`.  Less abstract machinery.
- Delegate c2w in packChars to Data.ByteString.Char8.packChars,
  this avoids costlier transformations of the stream.

#### Documentation [#42]

- In `foldr` docs remove erroneous claim that `foldr cons = id`
- Small improvements in function grouping of documentation and typo fix
- Drop signature comments from the export lists, they were not
  consistently there, and were sometimes wrong.  Too much effort to
  maintain for too little benefit.
- Fix some typos
- Make origin of 'io-streams` `Streams` module, `InputStream` type, more
  explicit.
- Use <BLANKLINE> in haddock for literal blanks in the output.

## 0.1.7 (2020-10-14)

Thanks to Viktor Dukhovni and Colin Woodbury for their contributions to this release.

#### Added

- The `skipSomeWS` function for efficiently skipping leading whitespace of both
  ASCII and non-ASCII.

#### Changed

- **The `ByteString` type has been renamed to `ByteStream`**. This fixes a
  well-reported confusion from users. An alias to the old name has been provided
  for back-compatibility, but is deprecated and be removed in the next major
  release.
- **Modules have been renamed** to match the precedent set by the main
  `streaming` library. Aliases to the old names have been provided, but will be
  removed in the next major release.
  - `Data.ByteString.Streaming` -> `Streaming.ByteString`
  - `Data.ByteString.Streaming.Char8` -> `Streaming.ByteString.Char8`
- An order-of-magnitude performance improvement in line splitting. [#18]
- Performance and correctness improvements for the `readInt` function. [#31]
- Documentation improved, and docstring coverage is now 100%. [#27]

#### Fixed

- An incorrect comment about `Handle`s being automatically closed upon EOF with
  `hGetContents` and `hGetContentsN`. [#9]
- A crash in `group` and `groupBy` when reading too many bytes. [#22]
- `groupBy` incorrectly ordering its output elements. [#4]

[#9]: https://github.com/haskell-streaming/streaming-bytestring/issues/9
[#18]: https://github.com/haskell-streaming/streaming-bytestring/pull/18
[#22]: https://github.com/haskell-streaming/streaming-bytestring/pull/22
[#4]: https://github.com/haskell-streaming/streaming-bytestring/issues/4
[#27]: https://github.com/haskell-streaming/streaming-bytestring/pull/27
[#31]: https://github.com/haskell-streaming/streaming-bytestring/pull/31

## 0.1.6

- `Semigroup` instance for `ByteString m r` added
- New function `lineSplit`

## 0.1.5

- Update for `streaming-0.2`
