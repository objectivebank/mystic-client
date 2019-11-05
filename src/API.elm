module API exposing (goalAreasQuery, objectivesSearch)

import API.Object
import API.Object.CategorizedObjectiveType as CategorizedObjectiveType
import API.Object.GoalAreaType as GoalAreaType
import API.Query as Query
import Data.Types exposing (GoalArea(..), Msg, Objective(..), UniqueID)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)


goalAreasQuery graphqlURL msgFunction =
    let
        request =
            Graphql.Http.queryRequest graphqlURL <| Query.goalAreas goalAreaSelection
    in
    Graphql.Http.send msgFunction request


goalAreaSelection : SelectionSet GoalArea API.Object.GoalAreaType
goalAreaSelection =
    SelectionSet.map2 StoredGoalArea
        GoalAreaType.id
        GoalAreaType.description


objectivesSearch : String -> (Result (Graphql.Http.Error (List Objective)) (List Objective) -> Msg) -> String -> List UniqueID -> Cmd Msg
objectivesSearch graphqlURL msgFunction searchText selectedGoalAreaIds =
    let
        request =
            Graphql.Http.queryRequest graphqlURL (objectivesQuery searchText selectedGoalAreaIds)
    in
    Graphql.Http.send msgFunction request


objectivesQuery : String -> List UniqueID -> SelectionSet (List Objective) RootQuery
objectivesQuery searchText selectedGoalAreas =
    let
        q =
            if String.length searchText > 0 then
                Present searchText

            else
                Absent

        goalAreaIds =
            if List.length selectedGoalAreas > 0 then
                Present selectedGoalAreas

            else
                Absent
    in
    Query.objectives { filter = { q = q, goalAreaIds = goalAreaIds } } objectivesSelection


objectivesSelection : SelectionSet Objective API.Object.CategorizedObjectiveType
objectivesSelection =
    SelectionSet.map4 StoredObjective
        CategorizedObjectiveType.id
        CategorizedObjectiveType.description
        CategorizedObjectiveType.goalAreaIds
        CategorizedObjectiveType.tagIds
