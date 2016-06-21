module Data.Aeson.Coerce where

import Data.Aeson as Aeson (Value(..), FromJSON, fromJSON, eitherDecode, Result(..))
import Data.ByteString.Lazy (ByteString)
import Data.Text (pack, unpack)

type LazyByteString = ByteString

class Coercable a where
  -- I'd love to have
  -- coerc :: a -> a
  asString :: a -> a
  asNumber :: a -> a

instance Coercable Value where
  asString (Number n) = String (pack $ show n)
  asString (Bool b)   = String (pack $ show b)
  asString _          = error "No coercion"
  asNumber (String s) = Number (read $ unpack s)
  asNumber (Bool b)   = Number $ if b then 1 else 0
  asNumber _          = error "No coercion"

eitherDecode :: FromJSON a => (Value -> Value) -> LazyByteString -> Either String a
eitherDecode ops bs = do
  val <- Aeson.eitherDecode bs
  resultToEither . fromJSON $ ops val
  where resultToEither :: Result a -> Either String a
        resultToEither (Error e) = Left e
        resultToEither (Success s) = Right s
