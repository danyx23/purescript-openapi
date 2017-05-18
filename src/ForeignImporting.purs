module ForeignImporting where

import Prelude
import Control.Bind ((=<<))
import Data.Foreign (F, Foreign, readNumber, readBoolean, readInt, readString, fail, ForeignError(..))
import Data.Foreign.Index ((!))
import Swagger (CollectionFormat(..), ParameterLocation(..), DefaultValue(..))

data Point = Point Number Number Number

instance showPoint :: Show Point where
  show (Point x y z) = "(Point " <> show [x, y, z] <> ")"

readPoint :: Foreign -> F Point
readPoint value = do
  Point
    <$> (value ! "x" >>= readNumber)
    <*> (value ! "y" >>= readNumber)
    <*> (value ! "z" >>= readNumber)


readCollectionFormat :: Foreign -> F CollectionFormat
readCollectionFormat value =
  value
    # readString >>=
      case _ of
        "csv" ->
          pure CommaSeparated

        "tsv" ->
          pure TabSeparated

        "ssv" ->
            pure SpaceSeparated

        "pipes" ->
            pure PipesSeparated

        "multiple" ->
            pure Multiple

        unknown ->
          fail $ ForeignError ("Found unknown parameter collection format: " <> unknown)


readParameterLocation :: Foreign -> F ParameterLocation
readParameterLocation value =
    value
      # readString >>=
        case _ of
          "query" ->
              pure Query

          "path" ->
              pure Path

          other ->
              fail $ ForeignError ("Unexpected in value " <> other)

composeWithMap :: forall a b c f
                  . Bind f
                  => Applicative f
                  => (a -> f b) -> (b -> c) -> (a -> f c)
composeWithMap func mapFunc =
  func >=> mapFunc >>> pure

infixr 1 composeWithMap as >=>>

readDefault :: String -> (Foreign -> F DefaultValue)
readDefault "integer" = readInt >=>> IntDefault
readDefault "float" = readNumber >=>> FloatDefault
readDefault "string" = readString >=>> StringDefault
readDefault "bool" = readBoolean >=>> BoolDefault
readDefault val = (\_ -> fail $ ForeignError ("Got unknown default type " <> val))
