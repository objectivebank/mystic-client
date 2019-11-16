module Main exposing (main)

import API exposing (goalAreasQuery, objectivesSearch)
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Data.Types exposing (ClientPronouns(..), Flags, Model, Msg(..), UniqueID, goalAreaId, objectiveId)
import Dict exposing (Dict)
import Ui.View exposing (..)
import Url exposing (Url)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> UrlRequest
        , onUrlChange = \_ -> UrlRequest
        }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags _ _ =
    ( { objectives = Dict.empty
      , objectiveSearchText = ""
      , matchingObjectives = []
      , selectedObjectives = []
      , goalAreas = Dict.empty
      , selectedGoalAreas = []
      , clientName = "Client"
      , clientPronouns = They
      , graphqlURL = flags.graphqlURL
      , searchInputEntered = False
      }
    , goalAreasQuery flags.graphqlURL GoalAreasResponse
    )


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddObjective id ->
            ( { model
                | selectedObjectives =
                    -- we implement Set behavior manually because we want to preserve order
                    if not <| List.member id model.selectedObjectives then
                        List.append model.selectedObjectives [ id ]

                    else
                        model.selectedObjectives
              }
            , Cmd.none
            )

        RemoveObjective id ->
            ( { model | selectedObjectives = List.filter (\item -> item /= id) model.selectedObjectives }, Cmd.none )

        UrlRequest ->
            ( model, Cmd.none )

        SearchTextEntered text ->
            let
                newText =
                    text

                interimMatching =
                    matchingForSearchInput model.matchingObjectives newText model.selectedGoalAreas
            in
            ( { model
                | objectiveSearchText = newText
                , matchingObjectives = interimMatching
                , searchInputEntered = searchInputEntered newText model.selectedObjectives
              }
            , if searchInputEntered newText model.selectedGoalAreas then
                objectivesSearch model.graphqlURL ObjectivesResponse newText model.selectedGoalAreas

              else
                Cmd.none
            )

        GoalAreaToggled id isSelected ->
            let
                newSelection =
                    if isSelected then
                        if not <| List.member id model.selectedGoalAreas then
                            List.append model.selectedGoalAreas [ id ]

                        else
                            model.selectedGoalAreas

                    else
                        List.filter (\item -> item /= id) model.selectedGoalAreas

                interimMatching =
                    matchingForSearchInput model.matchingObjectives model.objectiveSearchText newSelection
            in
            ( { model
                | selectedGoalAreas = newSelection
                , matchingObjectives = interimMatching
                , searchInputEntered = searchInputEntered model.objectiveSearchText newSelection
              }
            , if searchInputEntered model.objectiveSearchText newSelection then
                objectivesSearch model.graphqlURL ObjectivesResponse model.objectiveSearchText newSelection

              else
                Cmd.none
            )

        ClientNameUpdated newName ->
            ( { model | clientName = newName }, Cmd.none )

        ClientPronounsUpdated newPronouns ->
            ( { model | clientPronouns = newPronouns }, Cmd.none )

        GoalAreasResponse result ->
            ( { model | goalAreas = setGoalAreas result model.goalAreas }, Cmd.none )

        ObjectivesResponse result ->
            let
                ( newObjectives, newMatchingObjectives ) =
                    extractObjectivesResult result model.objectives
            in
            ( { model | objectives = newObjectives, matchingObjectives = newMatchingObjectives }
            , Cmd.none
            )


searchInputEntered : String -> List UniqueID -> Bool
searchInputEntered searchText selectedGoalAreaIds =
    String.length searchText > 0 || List.length selectedGoalAreaIds > 0


matchingForSearchInput : List UniqueID -> String -> List UniqueID -> List UniqueID
matchingForSearchInput currentMatchingObjectives searchText selectedGoalAreaIds =
    if searchInputEntered searchText selectedGoalAreaIds then
        currentMatchingObjectives

    else
        []


setGoalAreas result default =
    case result of
        Err _ ->
            default

        Ok goalAreas ->
            Dict.fromList <| List.map (\ga -> ( goalAreaId ga, ga )) goalAreas


extractObjectivesResult result current =
    case result of
        Err _ ->
            ( current, Dict.keys current )

        Ok objectives ->
            let
                new =
                    Dict.fromList <| List.map (\obj -> ( objectiveId obj, obj )) objectives

                combined =
                    Dict.union new current
            in
            ( combined, Dict.keys new )
