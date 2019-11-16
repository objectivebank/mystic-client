module Data.Types exposing (..)

import Common exposing (filterSecond)
import Dict exposing (Dict)
import Graphql.Http


type alias Model =
    { objectives : Dict UniqueID Objective
    , objectiveSearchText : String
    , matchingObjectives : List UniqueID
    , selectedObjectives : List UniqueID
    , goalAreas : Dict UniqueID GoalArea
    , selectedGoalAreas : List UniqueID
    , clientName : ClientName
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


type alias ClientName =
    String


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


goalAreaId : GoalArea -> UniqueID
goalAreaId ga =
    case ga of
        StoredGoalArea id _ ->
            id


goalAreaText : GoalArea -> String
goalAreaText ga =
    case ga of
        StoredGoalArea _ text ->
            text


objectiveId : Objective -> UniqueID
objectiveId obj =
    case obj of
        StoredObjective id _ _ _ ->
            id


objectiveText : Objective -> String
objectiveText obj =
    case obj of
        StoredObjective _ text _ _ ->
            text


objectiveGoalAreaIds : Objective -> List UniqueID
objectiveGoalAreaIds obj =
    case obj of
        StoredObjective _ _ goalAreaIds _ ->
            goalAreaIds


goalAreaDescription : GoalArea -> String
goalAreaDescription ga =
    case ga of
        StoredGoalArea _ description ->
            description


makeObjectiveCardData : Model -> List ObjectiveCardData
makeObjectiveCardData model =
    model.matchingObjectives
        |> List.map (\id -> ( id, Dict.get id model.objectives ))
        |> List.filterMap filterSecond
        |> List.map (foundObjectiveData model.clientName model.goalAreas model.selectedObjectives)


foundObjectiveData : String -> Dict UniqueID GoalArea -> List UniqueID -> ( UniqueID, Objective ) -> ObjectiveCardData
foundObjectiveData clientName goalAreas selectedObjectiveIds ( id, obj ) =
    { id = id
    , objective = interpolateClientAttributes clientName obj
    , selected = List.member id selectedObjectiveIds
    , goalAreaDescriptions = objectiveGoalAreas (objectiveGoalAreaIds <| obj) goalAreas
    }


interpolateClientAttributes : String -> Objective -> Objective
interpolateClientAttributes clientName obj =
    case obj of
        StoredObjective id description goalAreaIds tagIds ->
            StoredObjective id (String.replace "%1$s" clientName description) goalAreaIds tagIds


selectedObjectiveData : String -> Dict UniqueID GoalArea -> ( UniqueID, Objective ) -> ObjectiveCardData
selectedObjectiveData clientName goalAreas ( id, obj ) =
    { id = id
    , objective = interpolateClientAttributes clientName obj
    , selected = True
    , goalAreaDescriptions = objectiveGoalAreas (objectiveGoalAreaIds <| obj) goalAreas
    }


objectiveGoalAreas : List UniqueID -> Dict UniqueID GoalArea -> List GoalAreaDescription
objectiveGoalAreas goalAreaIds goalAreas =
    Dict.filter (\id _ -> List.member id goalAreaIds) goalAreas
        |> Dict.values
        |> List.map goalAreaDescription
