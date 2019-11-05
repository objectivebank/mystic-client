module Main exposing (main)

import API exposing (goalAreasQuery, objectivesSearch)
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Clippy exposing (clippy)
import Data.Types exposing (..)
import Dict exposing (Dict)
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attributes
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
      , graphqlURL = flags.graphqlURL
      , searchInputEntered = False
      }
    , goalAreasQuery flags.graphqlURL GoalAreasResponse
    )


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Objective Bank"
    , body =
        [ El.layout [] <| applicationView model
        ]
    }


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


applicationView : Model -> Element Msg
applicationView model =
    El.column
        [ El.width El.fill
        , El.height El.fill
        , El.padding 10
        ]
        [ searchBarView model.objectiveSearchText
        , middleView model
        , searchResultsView model
        ]


searchBarView : String -> Element Msg
searchBarView currentSearchText =
    El.row
        [ El.width El.fill
        , El.paddingEach { top = 10, right = 10, bottom = 10, left = 10 }
        ]
        [ El.column [ El.width El.fill, El.spacing 8 ]
            [ El.text "Objective Search"
            , Input.text [ El.width <| El.maximum 500 <| El.fill ]
                { onChange = \text -> SearchTextEntered text
                , text = currentSearchText
                , placeholder = Just (Input.placeholder [] <| El.text "Search...")
                , label = Input.labelHidden "Enter objective search terms"
                }
            ]
        ]


middleView : Model -> Element Msg
middleView model =
    El.row
        [ El.width El.fill
        , El.height <| El.px 500
        , El.paddingEach { top = 0, right = 10, bottom = 0, left = 10 }
        ]
        [ goalAreasView model
        , centerGap
        , selectedWrapper model
        ]


goalAreasView : Model -> Element Msg
goalAreasView model =
    El.column
        [ El.alignTop
        , El.width (El.fillPortion 7)
        , El.spaceEvenly
        , El.paddingEach { top = 10, right = 0, bottom = 10, left = 0 }
        ]
        [ El.text "Goal Areas"
        , goalAreaCheckboxes model.goalAreas model.selectedGoalAreas
        , clientVariablesView model
        ]


selectedWrapper : Model -> Element Msg
selectedWrapper model =
    let
        selectedObjectives =
            model.selectedObjectives
                |> List.map (\id -> ( id, Dict.get id model.objectives ))
                |> List.filterMap filterSecond

        objectiveCardData =
            selectedObjectives
                |> List.map (selectedObjectiveData model.clientName model.goalAreas)
    in
    El.column
        [ El.alignRight
        , El.width (El.fillPortion 10)
        , El.height El.fill
        , El.scrollbarY
        , El.paddingEach { top = 10, right = 0, bottom = 0, left = 10 }
        ]
        [ El.row [ El.height <| El.maximum 30 <| El.fill, El.width El.fill, El.spaceEvenly ]
            [ El.text <| selectedObjectivesHeading model.selectedObjectives
            , copyButton objectiveCardData
            ]
        , selectedView objectiveCardData
        ]


copyButton : List ObjectiveCardData -> Element Msg
copyButton objectiveCardData =
    let
        copyableObjectives =
            String.join "\n" <| List.map (objectiveText << .objective) objectiveCardData
    in
    Html.button
        [ Attributes.id "copy-button"
        , Attributes.title "Copy objectives"
        , Attributes.attribute "data-clipboard-text" copyableObjectives
        ]
        [ clippy "copy-button-image" ]
        |> El.html


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


selectedObjectivesHeading : List UniqueID -> String
selectedObjectivesHeading selectedObjectives =
    if List.length selectedObjectives == 0 then
        "Selected objectives"

    else
        "Selected objectives (" ++ String.fromInt (List.length selectedObjectives) ++ ")"


selectedView : List ObjectiveCardData -> Element Msg
selectedView objs =
    objectivesColumn lightBlue
        white
        (List.map
            (\objectiveCardData ->
                selectedObjective objectiveCardData.id
                    (objectiveText objectiveCardData.objective)
                    objectiveCardData.goalAreaDescriptions
            )
            objs
        )


centerGap : Element msg
centerGap =
    El.column [ El.width (El.fillPortion 1) ] []


goalAreaCheckboxes : Dict UniqueID GoalArea -> List UniqueID -> Element Msg
goalAreaCheckboxes goalAreas selectedGoalAreas =
    El.column [ El.paddingXY 0 10 ] <|
        (Dict.toList goalAreas
            |> List.map (\( id, ga ) -> goalAreaCheckbox id (List.member id selectedGoalAreas) (goalAreaText ga))
        )


goalAreaCheckbox : UniqueID -> Bool -> String -> Element Msg
goalAreaCheckbox id checked labelText =
    Input.checkbox []
        { onChange = GoalAreaToggled id
        , icon = Input.defaultCheckbox
        , checked = checked
        , label = Input.labelRight [] (El.text labelText)
        }


clientVariablesView : Model -> Element Msg
clientVariablesView model =
    El.row
        [ El.width El.fill
        , El.height El.fill
        , El.paddingXY 0 30
        ]
        [ clientNameInput model.clientName ]


clientNameInput : String -> Element Msg
clientNameInput currentName =
    Input.text [ El.width <| El.maximum 500 <| El.fill ]
        { onChange = ClientNameUpdated
        , text = currentName
        , placeholder = Nothing
        , label = Input.labelAbove [] <| El.text "Enter client name to populate objective descriptions:"
        }


searchResultsView : Model -> Element Msg
searchResultsView model =
    El.column
        [ El.width El.fill
        , El.paddingEach { top = 0, right = 10, bottom = 10, left = 10 }
        , El.spacing 24
        ]
        [ searchResults
            model.searchInputEntered
            (model.matchingObjectives
                |> List.map (\id -> ( id, Dict.get id model.objectives ))
                |> List.filterMap filterSecond
                |> List.map (foundObjectiveData model.clientName model.goalAreas model.selectedObjectives)
            )
        ]


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


filterSecond ( a, maybeB ) =
    case maybeB of
        Just b ->
            Just ( a, b )

        Nothing ->
            Nothing


searchResults : Bool -> List ObjectiveCardData -> Element Msg
searchResults isSearchInputEntered foundObjectives =
    El.row
        [ El.width El.fill
        , El.height El.fill
        ]
        [ objectivesRow isSearchInputEntered foundObjectives ]


objectivesRow : Bool -> List ObjectiveCardData -> Element Msg
objectivesRow isSearchInputEntered foundObjectives =
    let
        heading =
            if isSearchInputEntered then
                El.text "Results"

            else
                El.none
    in
    El.row
        [ El.spaceEvenly
        , El.width El.fill
        , El.height El.fill
        ]
        [ El.column [ El.width El.fill, El.spacing 10 ]
            [ heading, objectivesView foundObjectives ]
        ]


objectivesColumn : El.Color -> El.Color -> List (Element msg) -> Element msg
objectivesColumn borderColor backgroundColor elements =
    El.column
        [ Border.color borderColor
        , Background.color backgroundColor
        , Border.width 1
        , El.alignTop
        , El.width (El.fillPortion 8)
        , El.height El.fill
        , El.padding 8
        , El.spacing 8
        ]
        elements


objectivesView : List ObjectiveCardData -> Element Msg
objectivesView objs =
    let
        borderColor =
            if List.length objs > 0 then
                lightGray

            else
                white
    in
    objectivesColumn borderColor
        white
        (List.map
            (\objectiveCardData ->
                let
                    objectiveBackgroundColor =
                        if objectiveCardData.selected then
                            lightBlue

                        else
                            lightGray
                in
                foundObjective objectiveCardData.id
                    objectiveBackgroundColor
                    (objectiveText objectiveCardData.objective)
                    objectiveCardData.goalAreaDescriptions
            )
            objs
        )


lightGray =
    El.rgb 0.9 0.9 0.9


white =
    El.rgb 1 1 1


lightBlue =
    El.rgb255 204 229 255


foundObjective id backgroundColor text goalAreas =
    objectiveCard (AddObjective id) "Add" text goalAreas backgroundColor


selectedObjective id text goalAreas =
    objectiveCard (RemoveObjective id) "Remove" text goalAreas lightBlue


objectiveCard : Msg -> String -> String -> List GoalAreaDescription -> El.Color -> Element Msg
objectiveCard buttonMsg buttonText objText goalAreas backgroundColor =
    El.el
        [ El.padding 5
        , El.height <| El.px 90
        , El.width El.fill
        , Background.color
            backgroundColor
        ]
    <|
        El.column
            [ El.height El.fill, El.spaceEvenly, El.width El.fill ]
            [ El.row [ El.width El.fill, El.spaceEvenly ]
                [ El.paragraph [ El.width <| El.fillPortion 9 ] [ El.text objText ]
                , El.el [ El.width <| El.minimum 30 <| El.fillPortion 1, El.alignRight, El.alignTop ] (objectiveButton buttonMsg buttonText)
                ]
            , El.row [ El.width El.fill, El.spaceEvenly ]
                [ El.paragraph [ Font.size 14 ] [ El.text <| "Goal Areas: " ++ String.join ", " goalAreas ] ]
            ]


objectiveButton : Msg -> String -> Element Msg
objectiveButton msg buttonText =
    Input.button
        [ Background.color <| El.rgb 238 238 238
        ]
        { onPress = Just msg
        , label = El.text buttonText
        }
