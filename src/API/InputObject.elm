-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module API.InputObject exposing (..)

import API.Interface
import API.Object
import API.Scalar
import API.ScalarCodecs
import API.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


buildObjectiveFilterInput : (ObjectiveFilterInputOptionalFields -> ObjectiveFilterInputOptionalFields) -> ObjectiveFilterInput
buildObjectiveFilterInput fillOptionals =
    let
        optionals =
            fillOptionals
                { q = Absent, goalAreaIds = Absent }
    in
    { q = optionals.q, goalAreaIds = optionals.goalAreaIds }


type alias ObjectiveFilterInputOptionalFields =
    { q : OptionalArgument String
    , goalAreaIds : OptionalArgument (List Int)
    }


{-| Type for the ObjectiveFilterInput input object.
-}
type alias ObjectiveFilterInput =
    { q : OptionalArgument String
    , goalAreaIds : OptionalArgument (List Int)
    }


{-| Encode a ObjectiveFilterInput into a value that can be used as an argument.
-}
encodeObjectiveFilterInput : ObjectiveFilterInput -> Value
encodeObjectiveFilterInput input =
    Encode.maybeObject
        [ ( "q", Encode.string |> Encode.optional input.q ), ( "goalAreaIds", (Encode.int |> Encode.list) |> Encode.optional input.goalAreaIds ) ]
