## Unreleased

#### Changed

- An order-of-magnitude performance improvement in line splitting, thanks to
  Viktor Dukhovni. [#18]

#### Fixed

- An incorrect comment about `Handle`s being automatically closed upon EOF with
  `hGetContents` and `hGetContentsN`. [#9]
- A crash in `group` and `groupBy` when reading too many bytes. [#22]
- `groupBy` incorrectly ordering its output elements. [#4]

[#9]: https://github.com/haskell-streaming/streaming-bytestring/issues/9
[#18]: https://github.com/haskell-streaming/streaming-bytestring/pull/18
[#22]: https://github.com/haskell-streaming/streaming-bytestring/pull/22
[#4]: https://github.com/haskell-streaming/streaming-bytestring/issues/4

## 0.1.6

- `Semigroup` instance for `ByteString m r` added
- New function `lineSplit`

## 0.1.5

- Update for `streaming-0.2`
