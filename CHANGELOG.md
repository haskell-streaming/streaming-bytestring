## Unreleased

#### Changed

- Changed `for`'s callback to return `ByteStream m x`, to clarify that
  it is not used.

## 0.2.3 (2022-08-18)

#### Added

- Add `for :: Monad m => ByteStream m r -> (P.ByteString -> ByteStream m r) -> ByteStream m r`

## 0.2.2 (2022-05-18)

#### Changed

- Dependency adjustments.

## 0.2.1 (2021-06-23)

#### Changed

- Performance improvement when using GHC 9.

## 0.2.0 (2020-10-26)

**Note:** The deprecations added in `0.1.7` have _not_ been removed in this
version. Instead of `0.1.7`, that release should have been `0.2` in the first
place.

#### Added

- Add missing exports of `zipWithStream`, `materialize`, and `dematerialize`.

#### Changed

- **Breaking:** Switched names of `fold` and `fold_` in the non-`Char8` modules.
  The corresponding `Char8` functions and the rest of the library uses `_` for
  the variant that forgets the `r` value.
- **Breaking:** Unified `nextByte`/`nextChar` with `uncons`. The old `uncons`
  returned `Maybe` instead of the more natural `Either r`.
- **Breaking:** Similarly, `unconsChunk` and `nextChunk` have been unified.
- `nextByte`, `nextChar`, and `nextChunk` have been deprecated.
- Relaxed signature of `toStrict_` to allow any `r`, not just `()`.
- Permance improvements for `packChars` and `denull`.
- Various documentation improvements.
- Improved performance of `w8IsSpace` to more quickly filter out non-whitespace
  characters, and updated `words` to use it instead of the internal function
  `isSpaceWord8` from the `bytestring` package. See also
  [bytestring#315](https://github.com/haskell/bytestring/pull/315).

#### Fixed

- An edge case involving overflow in `readInt`.
- A potential crash in `uncons`.
- `intersperse` now ignores any initial empty chunks.
- `intercalate` now does not insert anything between the final substream and the
  outer stream end.
- `unlines` now correctly handles `Chunk "" (Empty r)` and `Empty r`.

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
