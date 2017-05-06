module BasicData where

import Prelude

import Data.Foreign (F, Foreign, readString)
import Data.Foreign.Index ((!))

newtype LambdaData = LambdaData { key1 :: String , key2 :: String }

instance showLambdaData :: Show LambdaData where
  show (LambdaData {key1, key2}) =
    "LambdaData { key1: " <> key1 <> ", key2: " <> key2 <> " }"

readLambdaData :: Foreign -> F LambdaData
readLambdaData value = do
  key1 <- value ! "key1" >>= readString
  key2 <- value ! "key2" >>= readString
  pure $ LambdaData {key1, key2}
