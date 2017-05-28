module Swagger where

import Data.Map (Map)
import Data.List (List)
import Data.Maybe (Maybe)

type Swagger schema parameter =
    { basePath :: String
    , definitions :: Map String schema
    , host :: String
    , info :: Info
    , parameters :: Map String (Parameter parameter)
    , paths :: Paths parameter schema
    , produces :: List String
    , schemes :: List String
    , swagger :: String
    }

type Info =
    { description :: String
    , title :: String
    , version :: String
    }


data SchemaOrReference
    = ReferencedSchema String
    | SpecifiedSchema (Schema SchemaOrReference)


data SchemaOnly
    = SchemaValue (Schema SchemaOnly)


unwrapSchemaValue :: SchemaOnly -> Schema SchemaOnly
unwrapSchemaValue (SchemaValue schema) =
    schema


data SimpleDataType
    = TypeInteger
    | TypeFloat
    | TypeString
    | TypeBoolean


type ObjectField schema =
    { fieldname :: String
    , content :: schema
    }


data DataType schema
    = TypeSimple SimpleDataType
    | TypeObject (List (ObjectField schema))
    | TypeArray schema


type Schema schema =
    { fieldType :: DataType schema
    , description :: Maybe String
    , xNullable :: Boolean
    , title :: Maybe String
    }


data ParameterLocation
    = Query
    | Path
    | Unknown


data CollectionFormat
    = CommaSeparated
    | SpaceSeparated
    | TabSeparated
    | PipesSeparated
    | Multiple


data DefaultValue
    = IntDefault Int
    | FloatDefault Number
    | BoolDefault Boolean
    | StringDefault String


data ParameterOrReference
    = ReferencedParameter String
    | SpecifiedParameter (Parameter ParameterOrReference)


data ParameterOnly
    = ParameterValue (Parameter ParameterOnly)


type Parameter parameter =
    { parameterType :: ParameterType parameter
    , name :: String
    , description :: Maybe String
    , inLocation :: ParameterLocation
    , required :: Boolean
    , default :: Maybe DefaultValue
    }

createParameter :: forall parameter .
                   ParameterType parameter ->
                   String ->
                   Maybe String ->
                   ParameterLocation ->
                   Boolean ->
                   Maybe DefaultValue ->
                   Parameter parameter
createParameter parameterType
                name
                description
                inLocation
                required
                default =
  { parameterType : parameterType
  , name : name
  , description : description
  , inLocation : inLocation
  , required : required
  , default : default
  }



data ParameterType parameter
    = ParameterTypeSimple SimpleDataType
    | ParameterTypeArray CollectionFormat parameter


type Paths parameter schema =
    Map String (Operations parameter schema)


type Operations parameter schema =
    { get :: Maybe (Operation parameter schema)
    }


type Operation parameter schema =
    { description :: Maybe String
    , parameters :: List parameter
    , responses :: Map String (Response schema)
    }


type Response schema =
    { description :: String
    , schema :: schema
    }

type SwaggerWithRefs =
    Swagger SchemaOrReference ParameterOrReference


type SwaggerWithoutRefs =
    Swagger SchemaOnly ParameterOnly


type DataTypeWithRefs =
    DataType SchemaOrReference


type DataTypeWithoutRefs =
    DataType SchemaOnly


type ObjectFieldWithRefs =
    ObjectField SchemaOrReference


type ObjectFieldWithoutRefs =
    ObjectField SchemaOnly


type SchemaWithRefs =
    Schema SchemaOrReference


type SchemaWithoutRefs =
    Schema SchemaOnly


type ParameterWithRefs =
    Parameter ParameterOrReference


type ParameterWithoutRefs =
    Parameter ParameterOnly


type ParameterTypeWithRefs =
    ParameterType ParameterOrReference


type ParameterTypeWithoutRefs =
    ParameterType ParameterOnly


type PathsWithRefs =
    Paths ParameterOrReference SchemaOrReference


type PathsWithoutRefs =
    Paths ParameterOnly SchemaOnly


type OperationsWithRefs =
    Operations ParameterOrReference SchemaOrReference


type OperationsWithoutRefs =
    Operations ParameterOnly SchemaOnly


type OperationWithRefs =
    Operation ParameterOrReference SchemaOrReference


type OperationWithoutRefs =
    Operation ParameterOnly SchemaOnly


type ResponseWithRefs =
    Response SchemaOrReference


type ResponseWithoutRefs =
    Response SchemaOnly
