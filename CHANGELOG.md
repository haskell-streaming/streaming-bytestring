## Unreleased

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
