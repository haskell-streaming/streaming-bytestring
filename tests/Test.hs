module Main ( main ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Streaming.Char8 as SBS8
import qualified Data.ByteString.Streaming.Internal as SBSI
import           Data.Functor.Identity
import qualified Data.List as L
import           Data.String (fromString)
import qualified Streaming as SM
import qualified Streaming.Prelude as S
import           System.IO
import           Test.SmallCheck.Series
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck
import           Text.Printf

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

fromChunks :: [String] -> SBS8.ByteString Identity ()
fromChunks = SBS8.fromChunks . S.each .  map BS8.pack

unix2dos :: String -> String
unix2dos = concatMap $ \c -> if c == '\n' then "\r\n" else [c]

unpackToString :: SBS8.ByteString Identity () -> String
unpackToString = runIdentity . S.toList_ . SBS8.unpack

sLines :: SBS8.ByteString Identity () -> [BS8.ByteString]
sLines
  = runIdentity
  . S.toList_
  . S.mapped SBS8.toStrict
  . SBS8.lines

noNullChunks :: S.Stream (SBS8.ByteString Identity) Identity () -> Bool
noNullChunks = SM.streamFold (\() -> True) runIdentity go
  where
  go :: SBS8.ByteString Identity Bool -> Bool
  go (SBSI.Empty b)           = b
  go (SBSI.Chunk bs sbs)      = not (BS8.null bs) && go sbs
  go (SBSI.Go (Identity sbs)) = go sbs

handleIsOpen :: Assertion
handleIsOpen = do
  h <- openBinaryFile "tests/sample.txt" ReadMode
  hIsOpen h >>= assertBool "Expected file handle to be open!"
  l <- SBS8.length_ $ SBS8.hGetContents h
  l @?= 73
  hIsOpen h >>= assertBool "Still expected file handle to be open!"

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "lines"
    [ testProperty "Data.ByteString.Streaming.Char8.lines is equivalent to Prelude.lines" $ over chunksSeries $ \chunks ->
        -- This only makes sure that the streaming-bytestring lines function
        -- matches the Prelude lines function when no carriage returns
        -- are present. They are not expected to have the same behavior
        -- with dos-style line termination.
        let expected = lines $ concat chunks
            got = (map BS8.unpack . sLines . fromChunks) chunks
        in
        if expected == got
          then Right ""
          else Left (printf "Expected %s; got %s" (show expected) (show got) :: String)
    , testProperty "lines recognizes DOS line endings" $ over strSeries $ \str ->
        sLines (SBS8.string $ unix2dos str) == sLines (SBS8.string str)
    , testProperty "lines recognizes DOS line endings with tiny chunks" $ over strSeries $ \str ->
        sLines (mapM_ SBS8.singleton $ unix2dos str) == sLines (mapM_ SBS8.singleton str)
    , testProperty "lineSplit does not create null chunks (LF)" $ over ((,) <$> nats <~> strSeries) $ \(n,str) ->
        noNullChunks (SBS8.lineSplit n (fromString str))
    , testProperty "lineSplit does not create null chunks (CRLF)" $ over ((,) <$> nats <~> strSeriesCrlf) $ \(n,str) ->
        noNullChunks (SBS8.lineSplit n (fromString str))
    , testProperty "concat after lineSplit round trips (LF)" $ over ((,) <$> nats <~> strSeries) $ \(n,str) ->
        unpackToString (SBS8.concat (SBS8.lineSplit n (fromString str))) == str
    , testProperty "concat after lineSplit round trips (CRLF)" $ over ((,) <$> nats <~> strSeriesCrlf) $ \(n,str) ->
        unpackToString (SBS8.concat (SBS8.lineSplit n (fromString str))) == str
    ]
  , testGroup "Unit Tests"
    [ testCase "hGetContents: Handle stays open" handleIsOpen
    ]
  ]
