module ForeignImporting where

import Prelude
import Control.Bind ((=<<))
import Control.Alt ((<|>))
import Data.Foreign (F, Foreign, readNumber, readNull, readBoolean, readInt, readString, fail, ForeignError(..))
import Data.Foreign.Index ((!))
import Swagger (CollectionFormat(..),
                ParameterLocation(..),
                DefaultValue(..),
                Info,
                ParameterType(..),
                ParameterOrReference(..),
                SimpleDataType(..),
                Parameter,
                createParameter)
import Data.Traversable (traverse)

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

decodeInfo :: Foreign -> F Info
decodeInfo f = do
  description <- f ! "description" >>= readString
  title <- f ! "title" >>= readString
  version <- f ! "version" >>= readString
  pure $ { description : description, title : title, version : version }


type Test = {bla :: String, blu :: String}

buildTest a b =
   {bla : a, blu: b}

testFn :: Test
testFn =
  buildTest "hello" "world"

{--
decodeSwagger :: Foreign -> F (Swagger SchemaOnly ParameterOnly)
decodeSwagger f = do
  basePath <-
--}

readParameterType :: String -> Foreign -> F (ParameterType ParameterOrReference)
readParameterType typeString f = do
  case typeString of
    "integer" -> pure $ ParameterTypeSimple TypeInteger
    "number" -> pure $ ParameterTypeSimple TypeFloat
    "string" -> pure $ ParameterTypeSimple TypeString
    "boolean" -> pure $ ParameterTypeSimple TypeBoolean
    "array" -> do
      collectionFormat <- f ! "collectionFormat" >>= readCollectionFormat
      items <- f ! "items" >>= readParameterOrReference
      pure $ ParameterTypeArray collectionFormat items
    _ -> fail $ ForeignError ("Unexpected type value " <> typeString)


readParameter :: Foreign -> F (Parameter ParameterOrReference)
readParameter f = do
    typeString <- f ! "type" >>= readString
    createParameter
      <$> (readParameterType typeString f)
      <*> (f ! "name" >>= readString)
      <*> (f ! "description" >>= readNull >>= (traverse readString))
      <*> (f ! "location" >>= readParameterLocation)
      <*> (f ! "required" >>= readBoolean)
      <*> (f ! "default" >>= readNull >>= (traverse $ readDefault typeString))


readParameterOrReference :: Foreign -> F ParameterOrReference
readParameterOrReference f =
  map ReferencedParameter (f ! "$ref" >>= readString )
  <|> do
    parameterType <- readParameter f
    pure $ SpecifiedParameter parameterType


{--
decodeParameterType : String -> (Foreign -> F (ParameterType ParameterOrReference))
decodeParameterType parameterType =
    case parameterType of
        "integer" ->
            pure $ ParameterTypeSimple TypeInteger

        "number" ->
            pure $ ParameterTypeSimple TypeFloat

        "string" ->
            pure $ ParameterTypeSimple TypeString

        "boolean" ->
            pure $ ParameterTypeSimple TypeBoolean

        "array" ->
                do

                "collectionFormat" ! (readString |> andThen decodeCollectionFormat))
                <*> ("items" := (lazy (\_ -> decodeParameterOrReference)))

        other ->
            fail <| "Found unrecognized parameter type " ++ other
--}
