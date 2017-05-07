module DefinitionDereferenceHelpers where

import Swagger
import Data.Map as Map
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.List (List(..), (:), null)
import Data.Maybe (Maybe(..))
import Data.Set (Set, singleton, empty, union, insert, fromFoldable)
import Data.Tuple (Tuple(..))
import Prelude (($), map, (#), (<>))

type InputDefinition =
    { definitionOrReference :: SchemaOrReference
    , references :: Set String
    }

type DereferencingMap a =
  { resolvedReferences :: Map.Map String a
  , unresolvableReferences :: Set String
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
              fieldsAsRefSets =
                  map (\field -> collectReferences field.content) fieldsList

              mergedRefSets =
                  foldr union empty (fromFoldable $ fieldsAsRefSets)
          in
              mergedRefSets

      TypeArray arraySchema ->
          collectReferences arraySchema


fillSchema :: DereferencingMapSchemas -> SchemaWithRefs -> Maybe SchemaWithoutRefs
fillSchema dereferencedDefinitions schema =
    case schema.fieldType of
        TypeSimple val ->
            Just $
            { fieldType : (TypeSimple val)
            , description : schema.description
            , xNullable : schema.xNullable
            , title : schema.title
            }

        TypeObject fields ->
            let
                dereferencedAndUndereferencedFields =
                  map (\{ fieldname, content } ->
                    let
                      maybeDereferencedField = fillSchemaReferences dereferencedDefinitions content

                      eitherResult =
                        case maybeDereferencedField of
                          Nothing ->
                            Left fieldname

                          Just dereferencedField ->
                            Right ({ fieldname : fieldname, content : SchemaValue dereferencedField })

                    in
                      eitherResult
                    )
                    fields

                foldFn :: forall a b . Tuple (List a) (List b) -> Either a b -> Tuple (List a) (List b)
                foldFn (Tuple alist blist) (Left aitem) = Tuple (aitem : alist) blist
                foldFn (Tuple alist blist) (Right bitem) = Tuple alist (bitem : blist)

                partitionList :: forall a b . List (Either a b) -> Tuple (List a) (List b)
                partitionList list =
                  foldl foldFn (Tuple Nil Nil) list

                -- We drop the unresolveable references here for now
                (Tuple unresolvedFieldNames dereferencedFields) =
                  partitionList dereferencedAndUndereferencedFields

                newFields =
                    TypeObject dereferencedFields
            in
                if null unresolvedFieldNames then
                  Just schema { fieldType = newFields }
                else
                  Nothing

        TypeArray arrayType ->
            let
                maybeNewArrayType =
                    fillSchemaReferences dereferencedDefinitions arrayType

                maybeNewArray =
                    map (\newArrayType -> TypeArray (SchemaValue newArrayType)) maybeNewArrayType

                maybeNewSchema =
                    map (\newArray -> schema { fieldType = newArray }) maybeNewArray
            in
                maybeNewSchema


fillSchemaReferences :: DereferencingMapSchemas -> SchemaOrReference -> Maybe SchemaWithoutRefs
fillSchemaReferences dereferencedDefinitions schemaOrRef =
    case schemaOrRef of
        ReferencedSchema ref ->
            let
                referenced =
                    Map.lookup ref dereferencedDefinitions.resolvedReferences
            in
                case referenced of
                    Nothing ->
                      Nothing

                    Just referencedSchema ->
                        Just referencedSchema

        SpecifiedSchema schema ->
            fillSchema dereferencedDefinitions schema


dereferenceDefinition :: Map.Map String InputDefinition -> DereferencingMapSchemas -> String -> InputDefinition -> DereferencingMapSchemas
dereferenceDefinition undereferencedDefs alreadyDereferenced defKey def  =
    case Map.lookup defKey alreadyDereferenced.resolvedReferences of
        Nothing ->
            let
                updatedDereferencedMap =
                  def.references
                    # foldl (\currentDereferenced ref ->
                        let
                          maybeItem = Map.lookup ref undereferencedDefs
                          updatedDereferenced =
                            case maybeItem of
                              Nothing ->
                                currentDereferenced { unresolvableReferences = insert ref currentDereferenced.unresolvableReferences }

                              Just item ->
                                let
                                  updatedMap =
                                    dereferenceDefinition undereferencedDefs currentDereferenced ref item

                                  --updatedMap =
                                  --  case dereferencedItem.dereferencedDefinition of
                                  --    Nothing ->
                                  --      currentDereferenced { unresolvableReferences = insert ref currentDereferenced.unresolvableReferences }
                                  --    Just dereferenedItem ->
                                  --      currentDereferenced { resolvedReferences = Map.insert ref dereferenedItem currentDereferenced.resolvedReferences }
                                in
                                  updatedMap

                        in
                          updatedDereferenced
                      )
                      alreadyDereferenced

                maybeFilledDef =
                    fillSchemaReferences updatedDereferencedMap def.definitionOrReference

                newDict =
                  case maybeFilledDef of
                    Nothing ->
                      updatedDereferencedMap { unresolvableReferences = insert defKey updatedDereferencedMap.unresolvableReferences }

                    Just filledDef ->
                      updatedDereferencedMap { resolvedReferences = Map.insert defKey filledDef updatedDereferencedMap.resolvedReferences }
            in
                newDict


        Just retrieved ->
            alreadyDereferenced

wrapInTuple :: forall b k v . (b -> k -> v -> b) -> (b -> Tuple k v -> b)
wrapInTuple fn =
  (\acc (Tuple key val) ->
    fn acc key val
  )

foldlWithKeys :: forall k v b . (b -> Tuple k v -> b) -> b -> Map.Map k v -> b
foldlWithKeys f acc m =
  m
  # (Map.toUnfoldable :: (Map.Map k v -> List (Tuple k v)))
  # foldl f acc

dereferenceDefinitions :: Map.Map String SchemaWithoutRefs -> Map.Map String SchemaOrReference -> DereferencingMapSchemas
dereferenceDefinitions dereferencedDefinitionsFromOtherSection definitions =
    let
        -- when we parse the swagger, the definitions dict will have keys like "Aggregations" but we need
        -- them to match the way they are referenced, i.e. "#/definitions/Aggregations"
        definitionsWithFullyReferencedKeys =
            foldlWithKeys (\newDict (Tuple key val) -> Map.insert ("#/definitions/" <> key) val newDict) Map.empty definitions

        inputDefinitions =
            Map.mapWithKey
                (\key val ->
                { definitionOrReference : val
                ,  references : (collectReferences val)
                })
                definitionsWithFullyReferencedKeys

        startAcc =
          { resolvedReferences : dereferencedDefinitionsFromOtherSection
          , unresolvableReferences : empty
          }

        --dereferencedFoldFn :: DereferencingMapSchemas -> Tuple String InputDefinition -> DereferencingMapSchema
        --dereferencedFoldFn acc (Tuple key def) =
        --  let
        --    dereferenceDefinition inputDefinitions
        --  in


        convertedDefinitions =
            foldlWithKeys (wrapInTuple $ dereferenceDefinition inputDefinitions) startAcc inputDefinitions

        --result =
        --    convertedDefinitions # Map.mapWithKey (\key val -> SchemaValue val)
    in
        convertedDefinitions
