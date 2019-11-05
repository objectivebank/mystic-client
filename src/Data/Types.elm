module Data.Types exposing (..)

import Dict exposing (Dict)
import Graphql.Http


type alias Model =
    { objectives : Dict UniqueID Objective
    , objectiveSearchText : String
    , matchingObjectives : List UniqueID
    , selectedObjectives : List UniqueID
    , goalAreas : Dict UniqueID GoalArea
    , selectedGoalAreas : List UniqueID
    , clientName : String
    , graphqlURL : String
    , searchInputEntered : Bool
    }


type Msg
    = NoOp
    | AddObjective UniqueID
    | RemoveObjective UniqueID
    | UrlRequest
    | SearchTextEntered String
    | GoalAreaToggled UniqueID Bool
    | ClientNameUpdated String
    | GoalAreasResponse (Result (Graphql.Http.Error (List GoalArea)) (List GoalArea))
    | ObjectivesResponse (Result (Graphql.Http.Error (List Objective)) (List Objective))


type alias UniqueID =
    Int


type alias GoalAreaDescription =
    String


type GoalArea
    = StoredGoalArea UniqueID GoalAreaDescription


type alias Flags =
    { graphqlURL : String }


type alias ObjectiveDescription =
    String


type Objective
    = StoredObjective UniqueID ObjectiveDescription (List UniqueID) (List UniqueID)


type alias ObjectiveCardData =
    { id : UniqueID
    , objective : Objective
    , selected : Bool
    , goalAreaDescriptions : List GoalAreaDescription
    }
