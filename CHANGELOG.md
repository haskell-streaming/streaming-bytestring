## Unreleased

#### Changed

- An order-of-magnitude performance improvement in line splitting, thanks to
  Viktor Dukhovni. [#18]

#### Fixed

- An incorrect comment about `Handle`s being automatically closed upon EOF with
  `hGetContents` and `hGetContentsN`. [#9]
- A critical bug in `group` and `groupBy` that would crash code when reading
  enough bytes. [#22]

[#9]: https://github.com/haskell-streaming/streaming-bytestring/issues/9
[#18]: https://github.com/haskell-streaming/streaming-bytestring/pull/18
[#22]: https://github.com/haskell-streaming/streaming-bytestring/pull/22

## 0.1.6

- `Semigroup` instance for `ByteString m r` added
- New function `lineSplit`

## 0.1.5

- Update for `streaming-0.2`
