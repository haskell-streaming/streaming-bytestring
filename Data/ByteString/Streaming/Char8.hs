{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.ByteString.Streaming.Char8
-- Copyright   : (c) Don Stewart 2006
--               (c) Duncan Coutts 2006-2011
--               (c) Michael Thompson 2015
-- License     : BSD-style
--
-- This library emulates "Data.ByteString.Lazy.Char8" but includes a monadic
-- element and thus at certain points uses a `Stream`/@FreeT@ type in place of
-- lists. See the documentation for @Data.ByteString.Streaming@ and the examples
-- of of use to implement simple shell operations
-- <https://gist.github.com/michaelt/6c6843e6dd8030e95d58 here>. Examples of use
-- with @http-client@, @attoparsec@, @aeson@, @zlib@ etc. can be found in the
-- 'streaming-utils' library.

module Data.ByteString.Streaming.Char8 (
    -- * The @ByteString@ type
    ByteString
    , Chunker

    -- * Introducing and eliminating 'ByteString's
    , empty            -- empty :: ByteString m ()
    , pack             -- pack :: Monad m => String -> ByteString m ()
    , unpack
    , string
    , unlines
    , unwords
    , singleton        -- singleton :: Monad m => Char -> ByteString m ()
    , fromChunks       -- fromChunks :: Monad m => Stream (Of ByteString) m r -> ByteString m r
    , fromLazy         -- fromLazy :: Monad m => ByteString -> ByteString m ()
    , fromStrict       -- fromStrict :: ByteString -> ByteString m ()
    , toChunks         -- toChunks :: Monad m => ByteString m r -> Stream (Of ByteString) m r
    , toLazy           -- toLazy :: Monad m => ByteString m () -> m ByteString
    , toLazy_
    , toStrict         -- toStrict :: Monad m => ByteString m () -> m ByteString
    , toStrict_
    , effects
    , copy
    , drained
    , mwrap

    -- * Transforming ByteStrings
    , map              -- map :: Monad m => (Char -> Char) -> ByteString m r -> ByteString m r
    , intercalate      -- intercalate :: Monad m => ByteString m () -> Stream (ByteString m) m r -> ByteString m r
    , intersperse      -- intersperse :: Monad m => Char -> ByteString m r -> ByteString m r

    -- * Basic interface
    , cons             -- cons :: Monad m => Char -> ByteString m r -> ByteString m r
    , cons'            -- cons' :: Char -> ByteString m r -> ByteString m r
    , snoc
    , append           -- append :: Monad m => ByteString m r -> ByteString m s -> ByteString m s
    , filter           -- filter :: (Char -> Bool) -> ByteString m r -> ByteString m r
    , head             -- head :: Monad m => ByteString m r -> m Char
    , head_            -- head' :: Monad m => ByteString m r -> m (Of Char r)
    , last             -- last :: Monad m => ByteString m r -> m Char
    , last_            -- last' :: Monad m => ByteString m r -> m (Of Char r)
    , null             -- null :: Monad m => ByteString m r -> m Bool
    , null_
    , testNull
    , nulls            -- null' :: Monad m => ByteString m r -> m (Of Bool r)
    , uncons           -- uncons :: Monad m => ByteString m r -> m (Either r (Char, ByteString m r))
    , nextChar

    -- * Substrings

    -- ** Breaking strings
    , break            -- break :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r)
    , drop             -- drop :: Monad m => GHC.Int.Int64 -> ByteString m r -> ByteString m r
    , dropWhile
    , group            -- group :: Monad m => ByteString m r -> Stream (ByteString m) m r
    , groupBy
    , span             -- span :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r)
    , splitAt          -- splitAt :: Monad m => GHC.Int.Int64 -> ByteString m r -> ByteString m (ByteString m r)
    , splitWith        -- splitWith :: Monad m => (Char -> Bool) -> ByteString m r -> Stream (ByteString m) m r
    , take             -- take :: Monad m => GHC.Int.Int64 -> ByteString m r -> ByteString m ()
    , takeWhile        -- takeWhile :: (Char -> Bool) -> ByteString m r -> ByteString m ()

    -- ** Breaking into many substrings
    , split            -- split :: Monad m => Char -> ByteString m r -> Stream (ByteString m) m r
    , lines
    , words
    , lineSplit
    , denull

    -- ** Special folds
    , concat          -- concat :: Monad m => Stream (ByteString m) m r -> ByteString m r

    -- * Builders

    , toStreamingByteString
    , toStreamingByteStringWith
    , toBuilder
    , concatBuilders

    -- * Building ByteStrings

    -- ** Infinite ByteStrings
    , repeat           -- repeat :: Char -> ByteString m ()
    , iterate          -- iterate :: (Char -> Char) -> Char -> ByteString m ()
    , cycle            -- cycle :: Monad m => ByteString m r -> ByteString m s

    -- ** Unfolding ByteStrings
    , unfoldr          -- unfoldr :: (a -> Maybe (Char, a)) -> a -> ByteString m ()
    , unfoldM          -- unfold  :: (a -> Either r (Char, a)) -> a -> ByteString m r
    , reread

    -- *  Folds, including support for `Control.Foldl`
--    , foldr            -- foldr :: Monad m => (Char -> a -> a) -> a -> ByteString m () -> m a
    , fold             -- fold :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m () -> m b
    , fold_            -- fold' :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m r -> m (b, r)
    , length
    , length_
    , count
    , count_
    , readInt
    , readInt'
    -- * I\/O with 'ByteString's

    -- ** Standard input and output
    , getContents      -- getContents :: ByteString IO ()
    , stdin            -- stdin :: ByteString IO ()
    , stdout           -- stdout :: ByteString IO r -> IO r
    , interact         -- interact :: (ByteString IO () -> ByteString IO r) -> IO r
    , putStr
    , putStrLn

    -- ** Files
    , readFile         -- readFile :: FilePath -> ByteString IO ()
    , writeFile        -- writeFile :: FilePath -> ByteString IO r -> IO r
    , appendFile       -- appendFile :: FilePath -> ByteString IO r -> IO r

    -- ** I\/O with Handles
    , fromHandle       -- fromHandle :: Handle -> ByteString IO ()
    , toHandle         -- toHandle :: Handle -> ByteString IO r -> IO r
    , hGet             -- hGet :: Handle -> Int -> ByteString IO ()
    , hGetContents     -- hGetContents :: Handle -> ByteString IO ()
    , hGetContentsN    -- hGetContentsN :: Int -> Handle -> ByteString IO ()
    , hGetN            -- hGetN :: Int -> Handle -> Int -> ByteString IO ()
    , hGetNonBlocking  -- hGetNonBlocking :: Handle -> Int -> ByteString IO ()
    , hGetNonBlockingN -- hGetNonBlockingN :: Int -> Handle -> Int -> ByteString IO ()
    , hPut             -- hPut :: Handle -> ByteString IO r -> IO r
--    , hPutNonBlocking  -- hPutNonBlocking :: Handle -> ByteString IO r -> ByteString IO r

    -- * Simple chunkwise operations
    , unconsChunk
    , nextChunk
    , chunk
    , foldrChunks
    , foldlChunks
    , chunkFold
    , chunkFoldM
    , chunkMap
    , chunkMapM
    , chunkMapM_

    -- * Etc.
--    , zipWithStream    -- zipWithStream :: Monad m => (forall x. a -> ByteString m x -> ByteString m x) -> [a] -> Stream (ByteString m) m r -> Stream (ByteString m) m r
    , distribute      -- distribute :: ByteString (t m) a -> t (ByteString m) a
    , materialize
    , dematerialize
  ) where

import           Prelude hiding
    (all, any, appendFile, break, concat, concatMap, cycle, drop, dropWhile,
    elem, filter, foldl, foldl1, foldr, foldr1, getContents, getLine, head,
    init, interact, iterate, last, length, lines, map, maximum, minimum,
    notElem, null, putStr, putStrLn, readFile, repeat, replicate, reverse,
    scanl, scanl1, scanr, scanr1, span, splitAt, tail, take, takeWhile,
    unlines, unwords, unzip, words, writeFile, zip, zipWith)
import qualified Prelude

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B

import           Streaming hiding (concats, distribute, unfold)
import           Streaming.Internal (Stream(..))
import qualified Streaming.Prelude as SP

import qualified Data.ByteString.Streaming as R
import           Data.ByteString.Streaming.Internal

import           Data.ByteString.Streaming
    (append, appendFile, concat, concatBuilders, cycle, denull, distribute,
    drained, drop, effects, empty, fromChunks, fromHandle, fromLazy,
    fromStrict, getContents, group, hGet, hGetContents, hGetContentsN, hGetN,
    hGetNonBlocking, hGetNonBlockingN, hPut, interact, intercalate, length,
    length_, nextChunk, null, null_, nulls, readFile, splitAt, stdin, stdout,
    take, testNull, toBuilder, toChunks, toHandle, toLazy, toLazy_,
    toStreamingByteString, toStreamingByteStringWith, toStrict, toStrict_,
    unconsChunk, writeFile)
 --   hPutNonBlocking,

import           Data.Bits (finiteBitSize) -- Since GHC 7.8
import           Data.Word (Word8)
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr
import           Foreign.Storable
import qualified System.IO as IO

-- | Given a stream of bytes, produce a vanilla `Stream` of characters.
unpack ::  Monad m => ByteString m r ->  Stream (Of Char) m r
unpack bs = case bs of
    Empty r    -> Return r
    Go m       -> Effect (fmap unpack m)
    Chunk c cs -> unpackAppendCharsLazy c (unpack cs)
  where
  unpackAppendCharsLazy :: B.ByteString -> Stream (Of Char) m r -> Stream (Of Char) m r
  unpackAppendCharsLazy (B.PS fp off len) xs
   | len <= 100 = unpackAppendCharsStrict (B.PS fp off len) xs
   | otherwise  = unpackAppendCharsStrict (B.PS fp off 100) remainder
   where
     remainder  = unpackAppendCharsLazy (B.PS fp (off+100) (len-100)) xs

  unpackAppendCharsStrict :: B.ByteString -> Stream (Of Char) m r -> Stream (Of Char) m r
  unpackAppendCharsStrict (B.PS fp off len) xs =
    B.accursedUnutterablePerformIO $ withForeignPtr fp $ \base -> do
         loop (base `plusPtr` (off-1)) (base `plusPtr` (off-1+len)) xs
     where
       loop !sentinal !p acc
         | p == sentinal = return acc
         | otherwise     = do x <- peek p
                              loop sentinal (p `plusPtr` (-1)) (Step (B.w2c x :> acc))
{-# INLINABLE unpack #-}

-- | /O(n)/ Convert a stream of separate characters into a packed byte stream.
pack :: Monad m => Stream (Of Char) m r -> ByteString m r
pack  = fromChunks
        . mapped (fmap (\(str :> r) -> Char8.pack str :> r) . SP.toList)
        . chunksOf 32
{-# INLINABLE pack #-}

-- | /O(1)/ Cons a 'Char' onto a byte stream.
cons :: Monad m => Char -> ByteString m r -> ByteString m r
cons c = R.cons (c2w c)
{-# INLINE cons #-}

-- | /O(1)/ Yield a 'Char' as a minimal 'ByteString'
singleton :: Monad m => Char -> ByteString m ()
singleton = R.singleton . c2w
{-# INLINE singleton #-}

-- | /O(1)/ Unlike 'cons', 'cons\'' is
-- strict in the ByteString that we are consing onto. More precisely, it forces
-- the head and the first chunk. It does this because, for space efficiency, it
-- may coalesce the new byte onto the first \'chunk\' rather than starting a
-- new \'chunk\'.
--
-- So that means you can't use a lazy recursive contruction like this:
--
-- > let xs = cons\' c xs in xs
--
-- You can however use 'cons', as well as 'repeat' and 'cycle', to build
-- infinite lazy ByteStrings.
--
cons' :: Char -> ByteString m r -> ByteString m r
cons' c (Chunk bs bss) | B.length bs < 16 = Chunk (B.cons (c2w c) bs) bss
cons' c cs             = Chunk (B.singleton (c2w c)) cs
{-# INLINE cons' #-}
--
-- | /O(n\/c)/ Append a byte to the end of a 'ByteString'
snoc :: Monad m => ByteString m r -> Char -> ByteString m r
snoc cs = R.snoc cs . c2w
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head_ :: Monad m => ByteString m r -> m Char
head_ = fmap w2c . R.head_
{-# INLINE head_ #-}

-- | /O(1)/ Extract the first element of a ByteString, if possible. Suitable for
-- use with `SP.mapped`:
--
-- @
-- S.mapped Q.head :: Stream (Q.ByteString m) m r -> Stream (Of (Maybe Char)) m r
-- @
head :: Monad m => ByteString m r -> m (Of (Maybe Char) r)
head = fmap (\(m:>r) -> fmap w2c m :> r) . R.head
{-# INLINE head #-}

-- | /O(n\/c)/ Extract the last element of a ByteString, which must be finite
-- and non-empty.
last_ :: Monad m => ByteString m r -> m Char
last_ = fmap w2c . R.last_
{-# INLINE last_ #-}

-- | Extract the last element of a `ByteString`, if possible. Suitable for use
-- with `SP.mapped`:
--
-- @
-- S.mapped Q.last :: Streaming (ByteString m) m r -> Stream (Of (Maybe Char)) m r
-- @
last :: Monad m => ByteString m r -> m (Of (Maybe Char) r)
last = fmap (\(m:>r) -> fmap w2c m :> r) . R.last
{-# INLINE last #-}

-- | The 'groupBy' function is a generalized version of 'group'.
groupBy :: Monad m => (Char -> Char -> Bool) -> ByteString m r -> Stream (ByteString m) m r
groupBy rel = R.groupBy (\w w' -> rel (w2c w) (w2c w'))
{-# INLINE groupBy #-}

-- | /O(1)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: Monad m => ByteString m r -> m (Either r (Char, ByteString m r))
uncons (Empty r) = return (Left r)
uncons (Chunk c cs)
    = return $ Right (w2c (B.unsafeHead c)
                     , if B.length c == 1
                         then cs
                         else Chunk (B.unsafeTail c) cs )
uncons (Go m) = m >>= uncons
{-# INLINABLE uncons #-}

-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@.
map :: Monad m => (Char -> Char) -> ByteString m r -> ByteString m r
map f = R.map (c2w . f . w2c)
{-# INLINE map #-}
--
-- -- | /O(n)/ 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- reverse :: ByteString -> ByteString
-- reverse cs0 = rev Empty cs0
--   where rev a Empty        = a
--         rev a (Chunk c cs) = rev (Chunk (B.reverse c) a) cs
-- {-# INLINE reverse #-}

-- | The 'intersperse' function takes a 'Char' and a 'ByteString' and
-- \`intersperses\' that byte between the elements of the 'ByteString'.
-- It is analogous to the intersperse function on Streams.
intersperse :: Monad m => Char -> ByteString m r -> ByteString m r
intersperse c = R.intersperse (c2w c)
{-# INLINE intersperse #-}

-- -- ---------------------------------------------------------------------
-- -- Reducing 'ByteString's

-- | 'fold_' keeps the return value of the left-folded bytestring. Useful for
-- simultaneous folds over a segmented bytestream.
fold_ :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m () -> m b
fold_ step begin done p0 = loop p0 begin
  where
    loop p !x = case p of
        Chunk bs bss -> loop bss $! Char8.foldl' step x bs
        Go    m      -> m >>= \p' -> loop p' x
        Empty _      -> return (done x)
{-# INLINABLE fold_ #-}

-- | Like `fold_`, but suitable for use with `S.mapped`.
fold :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m r -> m (Of b r)
fold step begin done p0 = loop p0 begin
  where
    loop p !x = case p of
        Chunk bs bss -> loop bss $! Char8.foldl' step x bs
        Go    m      -> m >>= \p' -> loop p' x
        Empty r      -> return (done x :> r)
{-# INLINABLE fold #-}

-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | @'iterate' f x@ returns an infinite ByteString of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]
iterate :: (Char -> Char) -> Char -> ByteString m r
iterate f c = R.iterate (c2w . f . w2c) (c2w c)
{-# INLINE iterate #-}

-- | @'repeat' x@ is an infinite ByteString, with @x@ the value of every
-- element.
repeat :: Char -> ByteString m r
repeat = R.repeat . c2w
{-# INLINE repeat #-}

-- -- | /O(n)/ @'replicate' n x@ is a ByteString of length @n@ with @x@
-- -- the value of every element.
-- --
-- replicate :: Int64 -> Word8 -> ByteString
-- replicate n w
--     | n <= 0             = Empty
--     | n < fromIntegral smallChunkSize = Chunk (B.replicate (fromIntegral n) w) Empty
--     | r == 0             = cs -- preserve invariant
--     | otherwise          = Chunk (B.unsafeTake (fromIntegral r) c) cs
--  where
--     c      = B.replicate smallChunkSize w
--     cs     = nChunks q
--     (q, r) = quotRem n (fromIntegral smallChunkSize)
--     nChunks 0 = Empty
--     nChunks m = Chunk c (nChunks (m-1))

-- | 'cycle' ties a finite ByteString into a circular one, or equivalently,
-- the infinite repetition of the original ByteString.
--
-- | /O(n)/ The 'unfoldM' function is analogous to the Stream \'unfoldr\'.
-- 'unfoldM' builds a ByteString from a seed value. The function takes the
-- element and returns 'Nothing' if it is done producing the ByteString or
-- returns 'Just' @(a,b)@, in which case, @a@ is a prepending to the ByteString
-- and @b@ is used as the next element in a recursive call.
unfoldM :: Monad m => (a -> Maybe (Char, a)) -> a -> ByteString m ()
unfoldM f = R.unfoldM go where
  go a = case f a of
    Nothing     -> Nothing
    Just (c,a') -> Just (c2w c, a')
{-# INLINE unfoldM #-}

-- | Given some pure process that produces characters, generate a stream of
-- bytes. The @r@ produced by the final `Left` will be the return value at the
-- end of the stream. Note also that the `Char` values will be truncated to
-- 8-bits.
unfoldr :: (a -> Either r (Char, a)) -> a -> ByteString m r
unfoldr step = R.unfoldr (either Left (\(c,a) -> Right (c2w c,a)) . step)
{-# INLINE unfoldr #-}

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m ()
takeWhile f  = R.takeWhile (f . w2c)
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m r
dropWhile f = R.dropWhile (f . w2c)
{-# INLINE dropWhile #-}

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r)
break f = R.break (f . w2c)
{-# INLINE break #-}

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r)
span p = break (not . p)
{-# INLINE span #-}

-- | Like `split`, but you can supply your own splitting predicate.
splitWith :: Monad m => (Char -> Bool) -> ByteString m r -> Stream (ByteString m) m r
splitWith f = R.splitWith (f . w2c)
{-# INLINE splitWith #-}

{- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
     argument, consuming the delimiter. I.e.

> split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
> split 'a'  "aXaXaXa"    == ["","X","X","X",""]
> split 'x'  "x"          == ["",""]

     and

> intercalate [c] . split c == id
> split == splitWith . (==)

As for all splitting functions in this library, this function does not copy the
substrings, it just constructs new 'ByteString's that are slices of the
original.

>>> Q.stdout $ Q.unlines $ Q.split 'n' "banana peel"
ba
a
a peel
-}
split :: Monad m => Char -> ByteString m r -> Stream (ByteString m) m r
split c = R.split (c2w c)
{-# INLINE split #-}

-- -- ---------------------------------------------------------------------
-- -- Searching ByteStrings

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m r
filter p = R.filter (p . w2c)
{-# INLINE filter #-}

-- | 'lines' turns a ByteString into a connected stream of ByteStrings at divide
-- at newline characters. The resulting strings do not contain newlines. This is
-- the genuinely streaming 'lines' which only breaks chunks, and thus never
-- increases the use of memory.
--
-- Because 'ByteString's are usually read in binary mode, with no line ending
-- conversion, this function recognizes both @\\n@ and @\\r\\n@ endings
-- (regardless of the current platform).
lines :: forall m r . Monad m => ByteString m r -> Stream (ByteString m) m r
lines text0 = loop1 text0
  where
    loop1 :: ByteString m r -> Stream (ByteString m) m r
    loop1 text =
      case text of
        Empty r -> Return r
        Go m -> Effect $ fmap loop1 m
        Chunk c cs
          | B.null c -> loop1 cs
          | otherwise -> Step (loop2 False text)
    loop2 :: Bool -> ByteString m r -> ByteString m (Stream (ByteString m) m r)
    loop2 prevCr text =
      case text of
        Empty r -> if prevCr
          then Chunk (B.singleton 13) (Empty (Return r))
          else Empty (Return r)
        Go m -> Go $ fmap (loop2 prevCr) m
        Chunk c cs ->
          case B.elemIndex 10 c of
            Nothing -> if B.null c
              then loop2 prevCr cs
              else if unsafeLast c == 13
                then Chunk (unsafeInit c) (loop2 True cs)
                else Chunk c (loop2 False cs)
            Just i -> do
              let prefixLength =
                    if i >= 1 && B.unsafeIndex c (i-1) == 13 -- \r\n (dos)
                      then i-1
                      else i
                  rest =
                    if B.length c > i+1
                      then Chunk (B.drop (i+1) c) cs
                      else cs
                  result = Chunk (B.unsafeTake prefixLength c) (Empty (loop1 rest))
              if i > 0 && prevCr
                then Chunk (B.singleton 13) result
                else result
{-# INLINABLE lines #-}

-- | The 'unlines' function restores line breaks between layers.
--
-- Note that this is not a perfect inverse of 'lines':
--
--  * @'lines' . 'unlines'@ can produce more strings than there were if some of
--  the \"lines\" had embedded newlines.
--
--  * @'unlines' . 'lines'@ will replace @\\r\\n@ with @\\n@.
unlines :: Monad m => Stream (ByteString m) m r ->  ByteString m r
unlines = loop where
  loop str =  case str of
    Return r -> Empty r
    Step bstr   -> do
      st <- bstr
      let bs = unlines st
      case bs of
        Chunk "" (Empty r)   -> Empty r
        Chunk "\n" (Empty _) -> bs
        _                    -> cons' '\n' bs
    Effect m  -> Go (fmap unlines m)
{-# INLINABLE unlines #-}

-- | 'words' breaks a byte stream up into a succession of byte streams
-- corresponding to words, breaking on 'Char's representing white space. This is
-- the genuinely streaming 'words'. A function that returns individual strict
-- bytestrings would concatenate even infinitely long words like @cycle "y"@ in
-- memory. When the stream is known to not contain unreasonably long words, you
-- can write @mapped toStrict . words@ or the like, if strict bytestrings are
-- needed.
words :: Monad m => ByteString m r -> Stream (ByteString m) m r
words = filtered . R.splitWith B.isSpaceWord8
 where
  filtered stream = case stream of
    Return r -> Return r
    Effect m -> Effect (fmap filtered m)
    Step bs  -> Effect $ bs_loop bs
  bs_loop bs = case bs of
      Empty r -> return $ filtered r
      Go m ->  m >>= bs_loop
      Chunk b bs' -> if B.null b
        then bs_loop bs'
        else return $ Step $ Chunk b (fmap filtered bs')
{-# INLINABLE words #-}

-- | The 'unwords' function is analogous to the 'unlines' function, on words.
unwords :: Monad m => Stream (ByteString m) m r -> ByteString m r
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}


{- | 'lineSplit' turns a ByteString into a connected stream of ByteStrings at
     divide after a fixed number of newline characters.
     Unlike most of the string splitting functions in this library,
     this function preserves newlines characters.

     Like 'lines', this function properly handles both @\\n@ and @\\r\\n@
     endings regardless of the current platform. It does not support @\\r@ or
     @\\n\\r@ line endings.

     >>> let planets = ["Mercury","Venus","Earth","Mars","Saturn","Jupiter","Neptune","Uranus"]
     >>> S.mapsM_ (\x -> putStrLn "Chunk" >> Q.putStrLn x) $ Q.lineSplit 3 $ Q.string $ L.unlines planets
     Chunk
     Mercury
     Venus
     Earth

     Chunk
     Mars
     Saturn
     Jupiter

     Chunk
     Neptune
     Uranus

     Since all characters originally present in the stream are preserved,
     this function satisfies the following law:

     > Ɐ n bs. concat (lineSplit n bs) ≅ bs
-}
lineSplit :: forall m r. Monad m
  => Int -- ^ number of lines per group
  -> ByteString m r -- ^ stream of bytes
  -> Stream (ByteString m) m r
lineSplit !n0 text0 = loop1 text0
  where
    n :: Int
    !n = max n0 1
    loop1 :: ByteString m r -> Stream (ByteString m) m r
    loop1 text =
      case text of
        Empty r -> Return r
        Go m -> Effect $ fmap loop1 m
        Chunk c cs
          | B.null c -> loop1 cs
          | otherwise -> Step (loop2 0 text)
    loop2 :: Int -> ByteString m r -> ByteString m (Stream (ByteString m) m r)
    loop2 !counter text =
      case text of
        Empty r -> Empty (Return r)
        Go m -> Go $ fmap (loop2 counter) m
        Chunk c cs ->
          case nthNewLine c (n - counter) of
            Left  !i -> Chunk c (loop2 (counter + i) cs)
            Right !l -> Chunk (B.unsafeTake l c)
                        $ Empty $ loop1 $! Chunk (B.unsafeDrop l c) cs
{-# INLINABLE lineSplit #-}

-- | Return either how many newlines a strict bytestring chunk contains, if
-- fewer than the number requested, or, else the total length of the requested
-- number of lines within the bytestring (equivalently, i.e. the start index of
-- the first /unwanted line/).
nthNewLine :: B.ByteString   -- input chunk
           -> Int            -- remaining number of newlines wanted
           -> Either Int Int -- Left count, else Right length
nthNewLine (B.PS fp off len) targetLines =
    B.accursedUnutterablePerformIO $ withForeignPtr fp $ \base ->
    loop (base `plusPtr` off) targetLines 0 len
  where
    loop :: Ptr Word8 -> Int -> Int -> Int -> IO (Either Int Int)
    loop !_ 0 !startIx !_ = return $ Right startIx
    loop !p !linesNeeded !startIx !bytesLeft = do
      q <- B.memchr p newline $ fromIntegral bytesLeft
      if q == nullPtr
      then return $ Left $! targetLines - linesNeeded
      else let !pnext = q `plusPtr` 1
               !skip  = pnext `minusPtr` p
               !snext = startIx + skip
               !bytes = bytesLeft - skip
            in loop pnext (linesNeeded - 1) snext bytes

newline :: Word8
newline = 10
{-# INLINE newline #-}

-- | Promote a vanilla `String` into a stream.
--
-- /Note:/ Each `Char` is truncated to 8 bits.
string :: String -> ByteString m ()
string = chunk . B.pack . Prelude.map B.c2w
{-# INLINE string #-}

-- | Returns the number of times its argument appears in the `ByteString`.
count_ :: Monad m => Char -> ByteString m r -> m Int
count_ c = R.count_ (c2w c)
{-# INLINE count_ #-}

-- | Returns the number of times its argument appears in the `ByteString`.
-- Suitable for use with `SP.mapped`:
--
-- @
-- S.mapped (Q.count \'a\') :: Stream (Q.ByteString m) m r -> Stream (Of Int) m r
-- @
count :: Monad m => Char -> ByteString m r -> m (Of Int r)
count c = R.count (c2w c)
{-# INLINE count #-}

-- | /O(1)/ Extract the head and tail of a 'ByteString', or its return value if
-- it is empty. This is the \'natural\' uncons for an effectful byte stream.
nextChar :: Monad m => ByteString m r -> m (Either r (Char, ByteString m r))
nextChar b = do
  e <- R.nextByte b
  case e of
    Left r       -> return $! Left r
    Right (w,bs) -> return $! Right (w2c w, bs)

-- | Print a stream of bytes to STDOUT.
putStr :: MonadIO m => ByteString m r -> m r
putStr = hPut IO.stdout
{-# INLINE putStr #-}

-- | Print a stream of bytes to STDOUT, ending with a final @\n@.
--
-- /Note:/ The final @\n@ is not added atomically, and in certain multi-threaded
-- scenarios might not appear where expected.
putStrLn :: MonadIO m => ByteString m r -> m r
putStrLn bs = hPut IO.stdout (snoc bs '\n')
{-# INLINE putStrLn #-}

-- | Type of a function that constructs a streaming ByteString from an
-- initial strict 'BC.ByteString' segment (possibly empty) and the rest
-- of the stream.  The simplest such function is the internal 'Chunk'
-- constructor which just prepends the strict segment to the stream.
--
-- A non-trivial transformation might prepare the stream for reading the
-- next element of a run of whitespace separated values, by removing
-- leading whitespace from its initial segment, and if that is then
-- empty typically also the rest of the stream (via 'dropWhile').  Note
-- that an unbounded stream consisting entirely of whitespace could then
-- block indefinitely when the its next character is requested.  If
-- that's a concern, the transformation may choose to trim just a
-- suitable bounded-length initial segment of the stream, and a suitable
-- error could subsequently be raised when unexpected leading whitespace
-- is encountered as a result of an unexpectedly long run of spaces.
--
type Chunker m r = (B.ByteString -> ByteString m r -> ByteString m r)
-- XXX: Bikeshed a better name?

-- | Try to read an 'Int' value from the 'ByteString', returning 'm
-- (Compose (Just n :> t))` on success, where @n@ is the value read and
-- @t@ is the rest of the input streaming 'ByteString'.  If the stream
-- of digits decodes to a value larger than can be represented by an
-- 'Int', the result will overflow and will not match the input digits.
-- Input is consumed until no more digits remain.  If the stream is an
-- unbounded run of decimal digits, this function may never return.  If
-- that's a concern, you can split the stream into an initial segment
-- with an apporpriate maximum length and a tail, and apply 'readInt' to
-- just the initial segment.
--
-- This function is just a wrapper around 'readInt'' with 'Chunk' as its
-- tail transformation function.
--
readInt :: Monad m
        => ByteString m r
        -> m (Compose (Of (Maybe Int)) (ByteString m) r)
{-# INLINE readInt #-}
readInt = readInt' Chunk

-- | Try to read an 'Int' value from the 'ByteString', returning 'm
-- (Compose (Just n :> t))' on success, where @n@ is the value read and
-- @t@ is the rest of the input streaming 'ByteString' computed by
-- applying the provided 'Chunker' function to the first non-empty chunk
-- following the parsed number and the rest of the stream.  The function
-- is not called, and the stream is returned as-is, preceding by
-- 'Nothing' for the ('Maybe' 'Int') result, when no number is found at
-- the start of the stream.
--
-- The 'Chunker' function can be used, for example, to discard trailing
-- whitespace, positioning the stream at the next potential input value.
-- It is up to the function to discard whitespace from any subsequent
-- chunks if the remainder of the first chunk is all whitespace, it is
-- not called recursively on its own output even when the result would
-- have an empty first chunk.
--
-- If the stream of digits decodes to a value larger than can be
-- represented by an 'Int', the result will overflow, and will not match
-- the input digits.  Input is consumed until no more digits remain.  If
-- the stream is an unbound run of decimal digits, this function may
-- never return.  If that's a concern, you can split the stream into an
-- initial segment with an apporpriate maximum length and a tail, and
-- apply 'readInt' to just the initial segment.
--
readInt' :: Monad m
         => Chunker m r    -- ^ 'Chunker' function for the tail of the stream
         -> ByteString m r -- ^ Input stream
         -> m (Compose (Of (Maybe Int)) (ByteString m) r)
{-# INLINE readInt' #-}
-- While dosigned, doplus and dominus are recursive and won't inline, it
-- pays to inline `readInt'`, because the body of the 'Chunker' can be
-- inlined into the recursive functions, substantially improving the
-- performance, e.g. when it is just 'Chunk' as in `readInt,` or when it
-- is function to skip trailing whitespace after the input (leading
-- whitespace in the tail) preparing the stream to read the next value.
readInt' chunker = dosigned
  where
    done !n !s = pure $ Compose $ Just n  :> s
    nada !s    = pure $ Compose $ Nothing :> s
    {-# INLINE done #-}
    {-# INLINE nada #-}

    -- Optimise for the case where the desired integer is present, and
    -- the first chunk is not empty or has just the sign, in which case
    -- 'Char8.readInt' gives a result, and we're done if more data
    -- remains in the chunk.  Otherwise, we have to deal with perhaps
    -- just the sign in the buffer, and/or nothing left in the buffer
    -- and the number might be continued in the next chunk.  Any
    -- continuation must no longer start with a sign, and we must negate
    -- any value returned before and after extending a negative number
    -- with more digits.
    --
    dosigned bs = case bs of
        Chunk c cs
            | not $ B.null c -> do
                let w = B.unsafeHead c
                if | Just (!n, t) <- Char8.readInt c
                     -> if | not (B.null t) -> done n (chunker t cs)
                           | w /= 0x2d      -> doplus n cs
                           | otherwise      -> dominus (-n) cs
                   | B.length c == 1
                     -> if | w == 0x2b     -> doplus 0 cs
                           | w == 0x2d     -> dominus 0 cs
                           | otherwise     -> nada bs
                   | otherwise -> nada bs
            | otherwise -> dosigned cs
        Go m -> m >>= dosigned
        e@(Empty _) -> nada e

    -- Finish reading a positive value, given the value of its prefix.
    -- For a 64-bit 'Int', only the last 64 digits matter.  Thus, if the
    -- new chunk has more than 64-bytes of initial digits, we don't need
    -- to combine the result with anything from the previous chunk.
    --
    -- But we still pay the cost of multiply by 10 and add for each
    -- digit, and typically the number of digits is small, so testing
    -- for long runs and using only the last 64 or so digits is likely
    -- slower in in practice for anything but unreasonably long digit
    -- sequences.  We just avoid paying the price again in the @10^l@
    -- term, when @l@ is large enough for the result to be sure to be
    -- zero modulo @2^N@ where @N@ is the number of bits in an 'Int'.
    --
    doplus !n str = case str of
        Chunk c cs  | B.null c -> doplus n cs
                    | B.unsafeHead c - 0x30 <= 9
                    , Just (!i, t) <- Char8.readInt c
                    , l <- B.length c - B.length t
                    , m <- if n /= 0 && l < finiteBitSize n
                           then n * 10^l + i
                           else i
                      -> if not (B.null t)
                         then done m (chunker t cs)
                         else doplus m cs
                    | otherwise -> done n $ chunker c cs
        Go m -> m >>= doplus n
        Empty _ -> done n str

    -- Finish reading a negative value, given the absolute value of its
    -- prefix.
    dominus !n str = case str of
        Chunk c cs  | B.null c -> dominus n cs
                    | B.unsafeHead c - 0x30 <= 9
                    , Just (!i, t) <- Char8.readInt c
                    , l <- B.length c - B.length t
                    , m <- if n /= 0 && l < finiteBitSize n
                           then n * 10^l + i
                           else i
                      -> if not (B.null t)
                         then done (-m) (chunker t cs)
                         else dominus m cs
                    | otherwise -> done (-n) $ chunker c cs
        Go m -> m >>= dominus n
        Empty _ -> done (-n) str
