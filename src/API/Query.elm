-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module API.Query exposing (..)

import API.InputObject
import API.Interface
import API.Object
import API.Scalar
import API.ScalarCodecs
import API.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)


apiVersion : SelectionSet String RootQuery
apiVersion =
    Object.selectionForField "String" "apiVersion" [] Decode.string


goalAreas : SelectionSet decodesTo API.Object.GoalAreaType -> SelectionSet (List decodesTo) RootQuery
goalAreas object_ =
    Object.selectionForCompositeField "goalAreas" [] object_ (identity >> Decode.list)


tags : SelectionSet decodesTo API.Object.TagType -> SelectionSet (List decodesTo) RootQuery
tags object_ =
    Object.selectionForCompositeField "tags" [] object_ (identity >> Decode.list)


type alias ObjectivesRequiredArguments =
    { filter : API.InputObject.ObjectiveFilterInput }


objectives : ObjectivesRequiredArguments -> SelectionSet decodesTo API.Object.CategorizedObjectiveType -> SelectionSet (List decodesTo) RootQuery
objectives requiredArgs object_ =
    Object.selectionForCompositeField "objectives" [ Argument.required "filter" requiredArgs.filter API.InputObject.encodeObjectiveFilterInput ] object_ (identity >> Decode.list)
