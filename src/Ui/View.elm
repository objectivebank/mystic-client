module Ui.View exposing (..)

import Browser exposing (Document)
import Clippy exposing (clippy)
import Common exposing (filterSecond)
import Data.Types exposing (Flags, GoalArea, GoalAreaDescription, Model, Msg(..), ObjectiveCardData, UniqueID, goalAreaText, makeObjectiveCardData, objectiveText, selectedObjectiveData)
import Dict exposing (Dict)
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attributes


view : Model -> Document Msg
view model =
    { title = "Objective Bank"
    , body =
        [ El.layout [] <| applicationView model <| makeObjectiveCardData model
        ]
    }


applicationView : Model -> List ObjectiveCardData -> Element Msg
applicationView model objectiveCardData =
    El.column
        [ El.width El.fill
        , El.height El.fill
        , El.padding 10
        ]
        [ searchBarView model.objectiveSearchText
        , middleView model
        , searchResultsView model.searchInputEntered objectiveCardData
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


clientNameInput : String -> Element Msg
clientNameInput currentName =
    Input.text [ El.width <| El.maximum 500 <| El.fill ]
        { onChange = ClientNameUpdated
        , text = currentName
        , placeholder = Nothing
        , label = Input.labelAbove [] <| El.text "Enter client name to populate objective descriptions:"
        }


searchResultsView : Bool -> List ObjectiveCardData -> Element Msg
searchResultsView isSearchInputEntered objectiveCardData =
    El.column
        [ El.width El.fill
        , El.paddingEach { top = 0, right = 10, bottom = 10, left = 10 }
        , El.spacing 24
        ]
        [ searchResults
            isSearchInputEntered
            objectiveCardData
        ]


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
