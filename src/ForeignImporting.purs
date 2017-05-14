module ForeignImporting where

import Prelude

import Data.Foreign (F, Foreign, readNumber, readBoolean, readInt, readString, fail, ForeignError(..))
import Data.Foreign.Index ((!))
import Swagger (CollectionFormat(..),
                ParameterLocation(..),
                DefaultValue(..)
                )

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

readDefault :: String -> Foreign -> F DefaultValue
readDefault paramType value =
  case paramType of
      "integer" ->
          readInt value >>= (\bla -> pure $ IntDefault bla)
          -- readInt >>> (map $ IntDefault)

      "number" ->
          map FloatDefault readNumber

      "string" ->
          map StringDefault readString

      "boolean" ->
          map BoolDefault readBoolean

      other ->
          fail $ ForeignError ("Got unknown default type " <> other)
