{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Streaming as Q
import qualified Data.ByteString.Streaming.Char8 as Q8
import qualified Data.ByteString.Streaming.Internal as QI
import           Data.Function (on)
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity
import qualified Data.List as L
import           Data.String (fromString)
import qualified Streaming as SM
import           Streaming (Of(..))
import qualified Streaming.Prelude as S
import           System.IO
import           Test.SmallCheck.Series
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck
import           Text.Printf (printf)

listOf :: Monad m => Series m a -> Series m [a]
listOf a = decDepth $
  pure [] \/ ((:) <$> a <~> listOf a)

strSeries :: Monad m => Series m String
strSeries = listOf (generate $ const ['a', 'b', '\n'])

strSeriesCrlf :: Monad m => Series m String
strSeriesCrlf = L.concat <$> listOf (generate $ const ["a", "b", "\r\n"])

chunksSeries :: Monad m => Series m [String]
chunksSeries = listOf strSeries

nats :: Monad m => Series m Int
nats = generate $ \d -> [1..d]

fromChunks :: [String] -> Q8.ByteString Identity ()
fromChunks = Q8.fromChunks . S.each .  map B.pack

unix2dos :: String -> String
unix2dos = concatMap $ \c -> if c == '\n' then "\r\n" else [c]

unpackToString :: Q8.ByteString Identity () -> String
unpackToString = runIdentity . S.toList_ . Q8.unpack

sLines :: Q8.ByteString Identity () -> [B.ByteString]
sLines
  = runIdentity
  . S.toList_
  . S.mapped Q8.toStrict
  . Q8.lines

noNullChunks :: S.Stream (Q8.ByteString Identity) Identity () -> Bool
noNullChunks = SM.streamFold (\() -> True) runIdentity go
  where
    go :: Q8.ByteString Identity Bool -> Bool
    go (QI.Empty b)           = b
    go (QI.Chunk bs sbs)      = not (B.null bs) && go sbs
    go (QI.Go (Identity sbs)) = go sbs

handleIsOpen :: Assertion
handleIsOpen = do
  h <- openBinaryFile "tests/sample.txt" ReadMode
  hIsOpen h >>= assertBool "Expected file handle to be open!"
  l <- Q8.length_ $ Q8.hGetContents h
  l @?= 73
  hIsOpen h >>= assertBool "Still expected file handle to be open!"

groupCrash :: Assertion
groupCrash = do
  a <- runResourceT . S.sum_ . SM.mapsM Q8.length . Q8.group $ Q8.readFile "tests/groupBy.txt"
  a @?= 39925
  b <- runResourceT . S.sum_ . SM.mapsM Q8.length . Q8.groupBy (\_ _ -> True) $ Q8.readFile "tests/groupBy.txt"
  b @?= 39925

groupCharOrder :: Assertion
groupCharOrder = do
  a <- S.toList_ . SM.mapsM Q8.toLazy $ Q8.group $ Q8.fromLazy "1234"
  a @?= (["1", "2", "3", "4"] :: [BL.ByteString])
  b <- S.toList_ . SM.mapsM Q8.toLazy $ Q8.group $ Q8.fromLazy "1122"
  b @?= (["11", "22"] :: [BL.ByteString])

groupByCharOrder :: Assertion
groupByCharOrder = do
  -- What about when everything fits into one group?
  y <- S.toList_ . SM.mapsM Q8.toLazy $ Q8.groupBy (\_ _ -> True) $ Q8.fromLazy "abcd"
  y @?= ["abcd"]
  -- Prove it's not an issue with the Char-based wrapper.
  z <- S.toList_ . SM.mapsM Q.toLazy $ Q.groupBy (\a b -> a - 1 == b) $ Q.fromLazy "98764321"
  z @?= ["98", "76", "43", "21"]
  -- Char-based variant
  a <- S.toList_ . SM.mapsM Q8.toLazy $ Q8.groupBy (\a b -> succ a == b) $ Q8.fromLazy "12346789"
  a @?= ["12", "34", "67", "89"]
  b <- S.toList_ . SM.mapsM Q8.toLazy $ Q8.groupBy (on (==) (== '5')) $ Q8.fromLazy "5678"
  b @?= ["5", "678"]

goodFindIndex :: Assertion
goodFindIndex = do
  assertBool "Expected the length of the string" $ QI.findIndexOrEnd (const False) "1234" == 4
  assertBool "Expected 0" $ QI.findIndexOrEnd (const True) "1234" == 0

firstI :: Assertion
firstI = do
  l <- runResourceT
    . S.length_                        -- IO Int
    . S.filter (== 'i')
    . S.concat                         -- Stream (Of Char) IO ()
    . S.mapped Q8.head                 -- Stream (Of (Maybe Char)) IO ()
    . Q8.denull                        -- Stream (ByteString IO) IO ()
    . Q8.lines                         -- Stream (Bytestring IO) IO ()
    $ Q8.readFile "tests/groupBy.txt"  -- ByteString IO ()
  l @?= 57

readIntCases :: Assertion
readIntCases = do
  ( Q8.readInt
    $ QI.Chunk "123"
    $ QI.Empty () ) >>= pure . getCompose >>= \case
    Just 123 :> bs -> Q.null_ bs >>= assertBool "Empty readInt1 tail"
    _              -> assertBool "Correct readInt1 value" False
  --
  ( Q8.readInt
    $ QI.Chunk "123"
    $ QI.Chunk "456"
    $ QI.Empty () ) >>= pure . getCompose >>= \case
    Just 123456 :> bs -> Q.null_ bs >>= assertBool "Empty readInt2 tail"
    _                 -> assertBool "Correct readInt2 value" False
  --
  ( Q8.readInt
    $ QI.Chunk "-123"
    $ QI.Chunk "456"
    $ QI.Empty () ) >>= pure . getCompose >>= \case
    Just (-123456) :> bs -> Q.null_ bs >>= assertBool "Empty readInt3 tail"
    _                    -> assertBool "Correct readInt3 value" False
  --
  ( Q8.readInt
    $ QI.Chunk "-123"
    $ QI.Chunk "456"
    $ QI.Chunk "789"
    $ QI.Chunk "-42"
    $ QI.Empty () ) >>= pure . getCompose >>= \case
    Just (-123456789) :> bs -> Q.toStrict_ bs >>=
                               assertBool "Correct readInt4 tail" . (== "-42")
    _                       -> assertBool "Correct readInt4 value" False
  --
  ( Q8.readInt
    $ QI.Chunk "-123"
    $ QI.Go $ pure
    $ QI.Chunk "456789123456789123456789123456789"
    $ QI.Chunk "+42"
    $ QI.Empty () ) >>= pure . getCompose >>= \case
    Just n :> bs -> do
                    let m = -123456789123456789123456789123456789 :: Integer
                        i = fromIntegral m
                    assertBool "Correct readInt5 value" $ n == i
                    Q.toStrict_ bs >>=
                        assertBool "Correct readInt5 tail" . (== "+42")
  --
  ( Q8.readInt' ((Q8.dropWhile (== ' ') .) . QI.Chunk)
    $ QI.Chunk "+123"
    $ QI.Go $ pure
    $ QI.Chunk "456789"
    $ QI.Chunk "123456789"
    $ QI.Go $ pure
    $ QI.Chunk "123456789"
    $ QI.Go $ pure
    $ QI.Go $ pure
    $ QI.Chunk "123456789 foo"
    $ QI.Empty () ) >>= pure . getCompose >>= \case
    Just n :> bs -> do
                    let m = 123456789123456789123456789123456789 :: Integer
                        i = fromIntegral m
                    assertBool "Correct readInt6' value" $ n == i
                    Q.toStrict_ bs >>=
                        assertBool "Correct readInt6' tail" . (== "foo")
  --
  ( Q8.readInt' ((Q8.dropWhile (== ' ') .) . QI.Chunk)
    $ QI.Chunk "-123"
    $ QI.Go $ pure
    $ QI.Chunk "456789"
    $ QI.Chunk "123456789"
    $ QI.Go $ pure
    $ QI.Chunk "123456789"
    $ QI.Chunk "123456789 "
    $ QI.Chunk "          "
    $ QI.Chunk "          "
    $ QI.Chunk "       -42"
    $ QI.Empty () ) >>= pure . getCompose >>= \case
    Just n :> bs -> do
                    let m = -123456789123456789123456789123456789 :: Integer
                        i = fromIntegral m
                    assertBool "Correct readInt7' value" $ n == i
                    Q.toStrict_ bs >>=
                        assertBool "Correct readInt7' tail" . (== "-42")
  --
  ( Q8.readInt' ((Q8.dropWhile (== ' ') .) . QI.Chunk)
    $ QI.Chunk "-123"
    $ QI.Go $ pure
    $ QI.Chunk "456789"
    $ QI.Chunk "123456789"
    $ QI.Go $ pure
    $ QI.Chunk "123456789"
    $ QI.Chunk "123456789 "
    $ QI.Chunk "          "
    $ QI.Chunk "          "
    $ QI.Chunk "          "
    $ QI.Empty 42 ) >>= pure . getCompose >>= \case
    Just n :> bs -> do
                    let m = -123456789123456789123456789123456789 :: Integer
                        i = fromIntegral m
                    assertBool "Correct readInt8' value" $ n == i
                    Q.toStrict bs >>= assertBool "Correct readInt8' tail" . (== ("" :> (42 :: Int)))
  --
  ( Q8.readInt' ((Q8.dropWhile (== ' ') .) . QI.Chunk)
    $ QI.Chunk "-123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Chunk "0123456789123456789123456789123456789123456789123456789123456789123456789"
    $ QI.Empty 42 ) >>= pure . getCompose >>= \case
    Just n :> bs -> do
                    -- The last 128 digits are "surely" enough.  (This test will break if
                    -- Haskell some day ends up with 256-bit 'Int's.
                    let m = -91234567891234567891234567891234567891234567891234567890123456789123456789123456789123456789123456789123456789123456789123456789 :: Integer
                        i = fromIntegral m
                    assertBool "Correct readInt9' value" $ n == i
                    Q.toStrict bs >>= assertBool "Correct readInt9' tail" . (== ("" :> (42 :: Int)))

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Property Tests"
    [ testProperty "Data.ByteString.Streaming.Char8.lines is equivalent to Prelude.lines" $ over chunksSeries $ \chunks ->
        -- This only makes sure that the streaming-bytestring lines function
        -- matches the Prelude lines function when no carriage returns
        -- are present. They are not expected to have the same behavior
        -- with dos-style line termination.
        let expected = lines $ concat chunks
            got = (map B.unpack . sLines . fromChunks) chunks
        in
        if expected == got
          then Right ("" :: String)
          else Left (printf "Expected %s; got %s" (show expected) (show got) :: String)
    , testProperty "lines recognizes DOS line endings" $ over strSeries $ \str ->
        sLines (Q8.string $ unix2dos str) == sLines (Q8.string str)
    , testProperty "lines recognizes DOS line endings with tiny chunks" $ over strSeries $ \str ->
        sLines (mapM_ Q8.singleton $ unix2dos str) == sLines (mapM_ Q8.singleton str)
    , testProperty "lineSplit does not create null chunks (LF)" $ over ((,) <$> nats <~> strSeries) $ \(n,str) ->
        noNullChunks (Q8.lineSplit n (fromString str))
    , testProperty "lineSplit does not create null chunks (CRLF)" $ over ((,) <$> nats <~> strSeriesCrlf) $ \(n,str) ->
        noNullChunks (Q8.lineSplit n (fromString str))
    , testProperty "concat after lineSplit round trips (LF)" $ over ((,) <$> nats <~> strSeries) $ \(n,str) ->
        unpackToString (Q8.concat (Q8.lineSplit n (fromString str))) == str
    , testProperty "concat after lineSplit round trips (CRLF)" $ over ((,) <$> nats <~> strSeriesCrlf) $ \(n,str) ->
        unpackToString (Q8.concat (Q8.lineSplit n (fromString str))) == str
    ]
  , testGroup "Unit Tests"
    [ testCase "hGetContents: Handle stays open" handleIsOpen
    , testCase "group(By): Don't crash" groupCrash
    , testCase "group: Char order" groupCharOrder
    , testCase "groupBy: Char order" groupByCharOrder
    , testCase "findIndexOrEnd" goodFindIndex
    , testCase "Stream Interop" firstI
    , testCase "readInt" readIntCases
    ]
  ]
