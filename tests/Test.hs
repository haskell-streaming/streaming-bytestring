{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Function (on)
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity
import qualified Data.IORef as IOR
import qualified Data.List as L
import           Data.String (fromString)
import           Streaming (Of(..))
import qualified Streaming as SM
import qualified Streaming.ByteString as Q
import qualified Streaming.ByteString.Char8 as Q8
import qualified Streaming.ByteString.Internal as QI
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

fromChunks :: [String] -> Q8.ByteStream Identity ()
fromChunks = Q8.fromChunks . S.each .  map B.pack

unix2dos :: String -> String
unix2dos = concatMap $ \c -> if c == '\n' then "\r\n" else [c]

unpackToString :: Q8.ByteStream Identity () -> String
unpackToString = runIdentity . S.toList_ . Q8.unpack

sLines :: Q8.ByteStream Identity () -> [B.ByteString]
sLines
  = runIdentity
  . S.toList_
  . S.mapped Q8.toStrict
  . Q8.lines

noNullChunks :: S.Stream (Q8.ByteStream Identity) Identity () -> Bool
noNullChunks = SM.streamFold (\() -> True) runIdentity go
  where
    go :: Q8.ByteStream Identity Bool -> Bool
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
    . Q8.denull                        -- Stream (ByteStream IO) IO ()
    . Q8.lines                         -- Stream (ByteStream IO) IO ()
    $ Q8.readFile "tests/groupBy.txt"  -- ByteStream IO ()
  l @?= 57

readIntCases :: Assertion
readIntCases = do
  let imax = maxBound :: Int
      imin = minBound :: Int
      imax1 = fromIntegral imax + 1 :: Integer
      imax10 = fromIntegral imax + 10 :: Integer
      imin1 = fromIntegral imin - 1 :: Integer
      imin10 = fromIntegral imin - 10 :: Integer
      smax = B.pack $ show imax
      smin = B.pack $ show imin
      smax1 = B.pack $ show imax1
      smax10 = B.pack $ show imax10
      smin1 = B.pack $ show imin1
      smin10 = B.pack $ show imin10
      maxfill = QI.defaultChunkSize
  cnt <- IOR.newIORef 0 -- number of effects in stream.
  -- Empty input
  IOR.writeIORef cnt 1
  res <- Q8.readInt
         $ QI.Chunk ""
         $ addEffect cnt
         $ QI.Chunk ""
         $ QI.Empty 0
  check cnt res Nothing ("" :> 0)
  -- Basic unsigned
  IOR.writeIORef cnt 1
  res <- Q8.readInt
         $ QI.Chunk "123"
         $ addEffect cnt
         $ QI.Empty 1
  check cnt res (Just 123) ("" :> 1)
  -- Basic negative
  IOR.writeIORef cnt 2
  res <- Q8.readInt
         $ QI.Chunk "-123"
         $ addEffect cnt
         $ QI.Chunk "456+789"
         $ addEffect cnt
         $ QI.Empty 2
  check cnt res (Just (-123456)) ("+789" :> 2)
  -- minBound with leading whitespace
  IOR.writeIORef cnt 4
  res <- readIntSkip
       $ QI.Chunk " \t\n\v\f\r\xa0"
       $ addEffect cnt
       $ QI.Chunk (B.take 4 smin)
       $ addEffect cnt
       $ QI.Chunk (B.drop 4 smin)
       $ addEffect cnt
       $ QI.Chunk "-42"
       $ addEffect cnt
       $ QI.Empty 3
  check cnt res (Just imin) ("-42" :> 3)
  -- maxBound with leading whitespace
  IOR.writeIORef cnt 4
  res <- readIntSkip
       $ QI.Chunk " \t\n\v\f\r\xa0"
       $ addEffect cnt
       $ QI.Chunk (B.take 4 smax)
       $ addEffect cnt
       $ QI.Chunk (B.drop 4 smax)
       $ addEffect cnt
       $ QI.Chunk "+42"
       $ addEffect cnt
       $ QI.Empty 4
  check cnt res (Just imax) ("+42" :> 4)
  -- minbound-1 with whitespace
  IOR.writeIORef cnt 4
  res <- readIntSkip
       $ QI.Chunk " \t\n\v\f\r\xa0"
       $ addEffect cnt
       $ QI.Chunk (B.take 4 smin1)
       $ addEffect cnt
       $ QI.Chunk (B.drop 4 smin1)
       $ addEffect cnt
       $ QI.Chunk ""
       $ addEffect cnt
       $ QI.Empty 5
  check cnt res Nothing (smin1 :> 5)
  -- maxbound+1 with whitespace
  IOR.writeIORef cnt 4
  res <- readIntSkip
       $ QI.Chunk " \t\n\v\f\r\xa0"
       $ addEffect cnt
       $ QI.Chunk (B.take 4 smax1)
       $ addEffect cnt
       $ QI.Chunk (B.drop 4 smax1)
       $ addEffect cnt
       $ QI.Chunk ""
       $ addEffect cnt
       $ QI.Empty 6
  check cnt res Nothing (smax1 :> 6)
  -- maxBound with explicit plus sign
  IOR.writeIORef cnt 2
  res <- readIntSkip
       $ QI.Chunk " +"
       $ addEffect cnt
       $ QI.Chunk smax
       $ QI.Chunk "tail"
       $ addEffect cnt
       $ QI.Empty 7
  check cnt res (Just imax) ("tail" :> 7)
  -- maxBound with almost excessive leading whitepace/zeros
  IOR.writeIORef cnt 4
  res <- readIntSkip
       $ QI.Chunk (B.replicate (maxfill-1) ' ')
       $ addEffect cnt
       $ QI.Chunk "   +"
       $ QI.Chunk (B.replicate (maxfill-1) '0')
       $ addEffect cnt
       $ QI.Chunk ("000000" `B.append` smax)
       $ addEffect cnt
       $ QI.Chunk "tail"
       $ addEffect cnt
       $ QI.Empty 8
  check cnt res (Just imax) ("tail" :> 8)
  -- (Exactly) too much leading whitespace
  IOR.writeIORef cnt 3
  res <- readIntSkip
       $ QI.Chunk (B.replicate maxfill ' ')
       $ addEffect cnt
       $ QI.Chunk " 1"
       $ addEffect cnt
       $ QI.Chunk ""
       $ addEffect cnt
       $ QI.Empty 9
  check cnt res Nothing (" 1" :> 9)
  -- (Exactly) too many leading zeros
  IOR.writeIORef cnt 3
  res <- readIntSkip
       $ QI.Chunk (B.replicate maxfill '0')
       $ addEffect cnt
       $ QI.Chunk "1"
       $ addEffect cnt
       $ QI.Chunk ""
       $ addEffect cnt
       $ QI.Empty 10
  check cnt res Nothing (B.replicate maxfill '0' `B.append` "1" :> 10)
  -- Bare plus
  IOR.writeIORef cnt 1
  res <- readIntSkip
       $ QI.Chunk "   +"
       $ addEffect cnt
       $ QI.Chunk "foo"
       $ QI.Empty 11
  check cnt res Nothing ("+foo" :> 11)
  -- Bare minus
  IOR.writeIORef cnt 1
  res <- readIntSkip
       $ QI.Chunk "   -"
       $ addEffect cnt
       $ QI.Chunk " bar"
       $ QI.Empty 12
  check cnt res Nothing ("- bar" :> 12)
  --
  IOR.writeIORef cnt 1
  let msg = "  nothing to see here move along  "
  res <- Q8.readInt
       $ QI.Chunk msg
       $ addEffect cnt
       $ QI.Empty 13
  check cnt res Nothing (msg :> 13)
  -- whitespace-only input
  IOR.writeIORef cnt 1
  res <- readIntSkip
         $ QI.Chunk " "
         $ addEffect cnt
         $ QI.Chunk "\n"
         $ QI.Empty 14
  check cnt res Nothing ("" :> 14)
  -- maxbound+10 with whitespace
  IOR.writeIORef cnt 4
  res <- readIntSkip
       $ QI.Chunk " \t\n\v\f\r\xa0"
       $ addEffect cnt
       $ QI.Chunk (B.take 4 smax10)
       $ addEffect cnt
       $ QI.Chunk (B.drop 4 smax10)
       $ addEffect cnt
       $ QI.Chunk ""
       $ addEffect cnt
       $ QI.Empty 15
  check cnt res Nothing (smax10 :> 15)
  -- minbound-10 with whitespace
  IOR.writeIORef cnt 4
  res <- readIntSkip
       $ QI.Chunk " \t\n\v\f\r\xa0"
       $ addEffect cnt
       $ QI.Chunk (B.take 4 smin10)
       $ addEffect cnt
       $ QI.Chunk (B.drop 4 smin10)
       $ addEffect cnt
       $ QI.Chunk ""
       $ addEffect cnt
       $ QI.Empty 16
  check cnt res Nothing (smin10 :> 16)
  where
    -- Count down to zero from initial value
    readIntSkip = Q8.readInt . Q8.skipSomeWS
    addEffect cnt str = QI.Go $ const str <$> IOR.modifyIORef' cnt pred
    check :: IOR.IORef Int
          -> Compose (Of (Maybe Int)) (QI.ByteStream IO) Int
          -> Maybe Int
          -> Of B.ByteString Int
          -> Assertion
    check cnt (Compose (gotInt :> str)) wantInt (wantStr :> wantR ) = do
        ( gotStr :> gotR ) <- Q.toStrict str
        c <- IOR.readIORef cnt
        assertBool ("Correct readInt effects " ++ show wantR) $ c == 0
        assertBool ("Correct readInt value " ++ show wantR ++ ": " ++ show gotInt) $ gotInt == wantInt
        assertBool ("Correct readInt tail " ++ show wantR ++ ": " ++ show gotStr) $ gotStr == wantStr
        assertBool ("Correct readInt residue " ++ show wantR ++ ": " ++ show gotR) $ gotR == wantR

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Property Tests"
    [ testProperty "Streaming.ByteString.Char8.lines is equivalent to Prelude.lines" $ over chunksSeries $ \chunks ->
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
