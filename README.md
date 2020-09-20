# streaming-bytestring

[![Build](https://github.com/haskell-streaming/streaming-bytestring/workflows/Tests/badge.svg)](https://github.com/haskell-streaming/streaming-bytestring/actions)
[![Build Status](https://travis-ci.org/haskell-streaming/streaming-bytestring.svg?branch=master)](https://travis-ci.org/haskell-streaming/streaming-bytestring)
[![Hackage](https://img.shields.io/hackage/v/streaming-bytestring.svg)](https://hackage.haskell.org/package/streaming-bytestring)

This library enables fast and safe streaming of byte data, in either `Word8` or
`Char` form. It is a core addition to the [`streaming`
ecosystem](https://github.com/haskell-streaming/) and avoids the usual pitfalls
of combinbing lazy `ByteString`s with lazy `IO`.

This library is used by
[`streaming-attoparsec`](http://hackage.haskell.org/package/streaming-attoparsec)
to enable vanilla [Attoparsec](http://hackage.haskell.org/package/attoparsec)
parsers to work with `streaming` "for free".

## Usage

### Importing and Types

Modules from this library are intended to be imported qualified. To avoid
conflicts with both the `bytestring` library and `streaming`, we recommended `Q`
as the qualified name:

```haskell
import qualified Data.ByteString.Streaming.Char8 as Q
```

Like the `bytestring` library, leaving off the `Char8` will expose an API based
on `Word8`. Following the philosophy of `streaming` that "the best API is the
one you already know", these APIs are based closely on `bytestring`. The core
type is `ByteString m r`, where:

- `m`: The Monad used to fetch further chunks from the "source", usually `IO`.
- `r`: The final return value after all streaming has concluded, usually `()` as in `streaming`.

You can imagine this type to represent an infinitely-sized collection of bytes,
although internally it references a **strict** `ByteString` no larger than 32kb,
followed by monadic instructions to fetch further chunks.

### Examples

#### File Input

To open a file of any size and count its characters:

```haskell
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Streaming.Char8 as Q

-- | Represents a potentially-infinite stream of `Char`.
chars :: ByteString IO ()
chars = Q.readFile "huge-file.txt"

main :: IO ()
main = runResourceT (Q.length_ chars) >>= print
```

Note that file IO specifically requires the
[`resourcet`](http://hackage.haskell.org/package/resourcet) library.

#### Line splitting and `Stream` interop

In the example above you may have noticed a lack of `Of` that we usually see
with `Stream`. Our old friend `lines` hints at this too:

```haskell
lines :: Monad m => ByteString m r -> Stream (ByteString m) m r
```

A stream-of-streams, yet no `Of` here either. The return type can't naively be
`Stream (Of ByteString) m r`, since the first line break might be at the very
end of a large file. Forcing that into a single strict `ByteString` would crash
your program.

To count the number of lines whose first letter is `i`:

```haskell
countOfI :: IO Int
countOfI = runResourceT
  . S.length_                   -- IO Int
  . S.filter (== 'i')           -- Stream (Of Char) IO ()
  . S.concat                    -- Stream (Of Char) IO ()
  . S.mapped Q.head             -- Stream (Of (Maybe Char)) IO ()
  . Q.lines                     -- Stream (Bytestring IO) IO ()
  $ Q.readFile "huge-file.txt"  -- ByteString IO ()
```

Critically, there are several functions which when combined with `mapped` can
bring us back into `Of`-land:

```haskell
head     :: Monad m => ByteString m r -> m (Of (Maybe Char) r)
last     :: Monad m => ByteString m r -> m (Of (Maybe Char) r)
null     :: Monad m => ByteString m r -> m (Of Bool) r)
count    :: Monad m => ByteString m r -> m (Of Int) r)
toLazy   :: Monad m => ByteString m r -> m (Of ByteString r) -- Be careful with this.
toStrict :: Monad m => ByteString m r -> m (Of ByteString r) -- Be even *more* careful with this.
```

When moving in the opposite direction API-wise, consider:

```haskell
fromChunks :: Stream (Of ByteString) m r -> ByteString m r
```
