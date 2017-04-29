module DefinitionDereferenceHelpers where

import Swagger
import Prelude (($), map)
import Data.List as List
import Data.Map as Map
import Control.Alt (alt)
import Control.Bind ((=<<))
import Data.Maybe (Maybe(..))
import Data.Set (Set, singleton, empty, union)
import Data.Tuple (Tuple(..))

type InputDefinition =
    { definitionOrReference :: SchemaOrReference
    , references :: Set String
    }

type DereferencingMap a =
  { resolvedReferences :: Map.Map String a
  , unresolvableKeys :: Set String
  }

type DereferencingMapSchemas =
  DereferencingMap SchemaWithoutRefs

collectReferences :: SchemaOrReference -> Set String
collectReferences (ReferencedSchema ref) =
  singleton ref

collectReferences (SpecifiedSchema schema) =
  case schema.fieldType of
      TypeSimple _ ->
          empty

      TypeObject fieldsList ->
          let
              fields =
                  Map.fromFoldable $ map (\{ fieldname, content } -> Tuple fieldname content ) fieldsList

              fieldsAsRefSets =
                  map (\key val -> collectReferences val) fields

              mergedRefSets =
                  List.foldr union empty (Map.values fieldsAsRefSets)
          in
              mergedRefSets

      TypeArray arraySchema ->
          collectReferences arraySchema


fillSchema :: DereferencingMapSchemas -> SchemaWithRefs -> SchemaWithoutRefs
fillSchema dereferencedDefinitions schema =
    case schema.fieldType of
        TypeSimple val ->
            { fieldType : (TypeSimple val)
            , description : schema.description
            , xNullable : schema.xNullable
            , title : schema.title
            }

        TypeObject fields ->
            let
                dereferencedFields =
                    map (\{ fieldname, content } -> { fieldname : fieldname, content : SchemaValue $ (fillSchemaReferences dereferencedDefinitions content)}) fields

                newFields =
                    TypeObject dereferencedFields
            in
                schema { fieldType = newFields }

        TypeArray arrayType ->
            let
                newArrayType =
                    fillSchemaReferences dereferencedDefinitions arrayType

                newArray =
                    TypeArray (SchemaValue newArrayType)
            in
                schema { fieldType = newArray }


fillSchemaReferences :: DereferencingMapSchemas -> SchemaOrReference -> Maybe SchemaWithoutRefs
fillSchemaReferences dereferencedDefinitions schemaOrRef =
    case schemaOrRef of
        ReferencedSchema ref ->
            let
                referenced =
                    Map.lookup ref dereferencedDefinitions
            in
                case referenced of
                    Nothing ->
                      Nothing

                    Just referencedSchema ->
                        Just referencedSchema

        SpecifiedSchema schema ->
            Just $ fillSchema dereferencedDefinitions schema


dereferenceDefinition :: Map.Map String InputDefinition -> String -> InputDefinition -> Map.Map String SchemaWithoutRefs -> Map.Map String SchemaWithoutRefs
dereferenceDefinition undereferencedDefs defKey def alreadyDereferenced =
    case Dict.get defKey alreadyDereferenced of
        Nothing ->
            let
                requiredReferences =
                    def.references
                        |> Set.toList
                        |> List.map (\ref -> Tuple ref (Map.get ref undereferencedDefs))
                        |> List.filterMap
                            (\Tuple ref maybeItem  ->
                                case maybeItem of
                                    Nothing ->
                                        Nothing

                                    Just item ->
                                        Just (Tuple ref item )
                            )
                        |> Dict.fromList
                        |> Dict.foldl (dereferenceDefinition undereferencedDefs) alreadyDereferenced

                dereferencesSoFar =
                    Dict.union alreadyDereferenced requiredReferences

                filledDef =
                    fillSchemaReferences dereferencesSoFar def.definitionOrReference

                newDict =
                    Dict.insert defKey filledDef dereferencesSoFar
            in
                newDict

        Just _ ->
            alreadyDereferenced


dereferenceDefinitions :: Map.Map String SchemaWithoutRefs -> Map.Map String SchemaOrReference -> Map.Map String SchemaOnly
dereferenceDefinitions dereferencedDefinitionsFromOtherSection definitions =
    let
        -- when we parse the swagger, the definitions dict will have keys like "Aggregations" but we need
        -- them to match the way they are referenced, i.e. "#/definitions/Aggregations"
        definitionsWithFullyReferencedKeys =
            Dict.foldl (\key val newDict -> Dict.insert ("#/definitions/" ++ key) val newDict) Dict.empty definitions

        inputDefinitions =
            Dict.map (\key val -> InputDefinition val (collectReferences val)) definitionsWithFullyReferencedKeys

        convertedDefinitions =
            Dict.foldl (dereferenceDefinition inputDefinitions) dereferencedDefinitionsFromOtherSection inputDefinitions

        result =
            convertedDefinitions |> Dict.map (\key val -> SchemaValue val)
    in
        result
