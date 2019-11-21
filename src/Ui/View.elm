module Ui.View exposing (..)

import Browser exposing (Document)
import Clippy exposing (clippy)
import Common exposing (filterSecond)
import Data.Types exposing (ClientName, ClientPronouns(..), Flags, GoalArea, GoalAreaDescription, Model, Msg(..), ObjectiveCardData, UniqueID, goalAreaText, makeObjectiveCardData, objectiveText, selectedObjectiveData)
import Dict exposing (Dict)
import Element as El exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attributes


view : Model -> Document Msg
view model =
    { title = "Objective Bank"
    , body = [ El.layout [] <| applicationView model ]
    }


lightGray =
    El.rgb 0.9 0.9 0.9


white =
    El.rgb 1 1 1


lightBlue =
    El.rgb255 204 229 255


panelPadding =
    8


panelSpacing =
    2


controlPanelHeight =
    700


outerPadding =
    6


panelLabelHeight =
    30


panelLabelPadding =
    { top = 0
    , right = 0
    , left = 0
    , bottom = 10
    }


applicationView : Model -> Element Msg
applicationView model =
    let
        selectedObjectives =
            model.selectedObjectives
                |> List.map (\id -> ( id, Dict.get id model.objectives ))
                |> List.filterMap filterSecond

        selectedObjectivesCardData =
            selectedObjectives
                |> List.map (selectedObjectiveData model.clientName model.clientPronouns model.goalAreas)
    in
    El.column
        [ El.width El.fill
        , El.height El.fill
        , El.padding outerPadding
        ]
        [ El.row
            [ El.width El.fill
            , El.height <| El.px controlPanelHeight
            ]
            [ El.column
                [ El.width <| El.fillPortion 3
                , El.height El.fill
                , El.padding panelPadding
                , El.spacing panelSpacing
                ]
                [ El.row
                    [ El.width El.fill
                    , El.height <| El.px panelLabelHeight
                    , El.paddingEach panelLabelPadding
                    ]
                    [ objectiveSearchLabelView
                    ]
                , El.row
                    [ El.width El.fill
                    , El.height <| El.fillPortion 2
                    ]
                    [ El.el
                        [ El.width El.fill
                        , El.alignTop
                        ]
                      <|
                        objectiveSearchBarView model.objectiveSearchText
                    ]
                , El.row
                    [ El.width El.fill
                    , El.height <| El.fillPortion 14
                    ]
                    [ goalAreasView model ]
                , El.row
                    [ El.width El.fill
                    , El.height <| El.fillPortion 6
                    ]
                    [ clientVariablesView model ]
                ]
            , El.column
                [ El.width <| El.fillPortion 5
                , El.height El.fill
                , El.padding panelPadding
                , El.spacing panelSpacing
                ]
                [ El.row
                    [ El.width El.fill
                    , El.height <| El.px 30
                    , El.paddingEach panelLabelPadding
                    , El.spaceEvenly
                    ]
                    (selectedObjectivesCopyView model selectedObjectivesCardData)
                , El.row
                    [ El.width El.fill
                    , El.height El.fill
                    , El.scrollbarY
                    ]
                    [ El.column
                        [ El.width El.fill
                        , El.height El.fill
                        , El.spacing panelSpacing
                        ]
                        [ selectedObjectivesView selectedObjectivesCardData ]
                    ]
                ]
            ]
        , El.row
            [ El.width El.fill
            , El.height El.fill
            ]
            [ El.column
                [ El.width El.fill
                , El.height El.fill
                , El.padding panelPadding
                , El.spacing panelSpacing
                ]
                [ foundObjectivesHeading model.searchInputEntered
                , searchResultsView <| makeObjectiveCardData model
                ]
            ]
        ]


objectiveSearchLabelView =
    El.text "Objective Search"


objectiveSearchBarView currentSearchText =
    Input.text [ El.width El.fill ]
        { onChange = \text -> SearchTextEntered text
        , text = currentSearchText
        , placeholder = Just (Input.placeholder [] <| El.text "Search...")
        , label = Input.labelHidden "Enter objective search terms"
        }


goalAreasView : Model -> Element Msg
goalAreasView model =
    El.column
        []
        [ El.text "Goal Areas"
        , goalAreaCheckboxes model.goalAreas model.selectedGoalAreas
        ]


clientVariablesView : Model -> Element Msg
clientVariablesView model =
    El.column
        [ El.width El.fill
        , El.height El.fill
        , El.spaceEvenly
        ]
        [ El.row
            [ El.width El.fill
            , El.height <| El.fillPortion 2
            ]
            [ clientNameInput model.clientName ]
        , El.row
            [ El.width El.fill
            , El.height <| El.fillPortion 3
            ]
            [ clientPronounsInput model.clientPronouns ]
        ]


selectedObjectivesCopyView : Model -> List ObjectiveCardData -> List (Element Msg)
selectedObjectivesCopyView model objectiveCardData =
    [ El.text <| selectedObjectivesHeading model.selectedObjectives
    , copyButton objectiveCardData
    ]


selectedObjectivesHeading : List UniqueID -> String
selectedObjectivesHeading selectedObjectives =
    if List.length selectedObjectives == 0 then
        "Selected objectives"

    else
        "Selected objectives (" ++ String.fromInt (List.length selectedObjectives) ++ ")"


selectedObjectivesView : List ObjectiveCardData -> Element Msg
selectedObjectivesView objs =
    objectivesColumn
        white
        (List.map
            (\objectiveCardData ->
                selectedObjective objectiveCardData.id
                    (objectiveText objectiveCardData.objective)
                    objectiveCardData.goalAreaDescriptions
            )
            objs
        )


foundObjectivesHeading : Bool -> Element Msg
foundObjectivesHeading isSearchInputEntered =
    if isSearchInputEntered then
        El.text "Results"

    else
        El.none


searchResultsView : List ObjectiveCardData -> Element Msg
searchResultsView foundObjectives =
    El.row
        [ El.spaceEvenly
        , El.width El.fill
        , El.height El.fill
        ]
        [ El.column [ El.width El.fill, El.spacing 10 ]
            [ objectivesView foundObjectives ]
        ]


objectivesView : List ObjectiveCardData -> Element Msg
objectivesView objs =
    objectivesColumn
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


objectivesColumn : El.Color -> List (Element msg) -> Element msg
objectivesColumn backgroundColor elements =
    El.column
        [ Background.color backgroundColor
        , El.alignTop
        , El.width El.fill
        , El.height El.fill
        , El.spacing 8
        ]
        elements


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
        , Attributes.style "background-color" "unset"
        , Attributes.style "padding-top" "3px"
        , Attributes.style "border-color" "rgb(204, 229, 255)"
        ]
        [ clippy "copy-button-image" ]
        |> El.html


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


clientNameInput : ClientName -> Element Msg
clientNameInput currentName =
    Input.text [ El.width El.fill ]
        { onChange = ClientNameUpdated
        , text = currentName
        , placeholder = Nothing
        , label = Input.labelAbove [] <| El.text "Enter client name to populate objective descriptions:"
        }


clientPronounsInput : ClientPronouns -> Element Msg
clientPronounsInput currentPronouns =
    Input.radio []
        { onChange = ClientPronounsUpdated
        , selected = Just currentPronouns
        , label = Input.labelAbove [] <| El.text "Client pronouns:"
        , options =
            [ Input.option He (El.text "He")
            , Input.option She (El.text "She")
            ]
        }


foundObjective id backgroundColor text goalAreas =
    objectiveCard (AddObjective id) "Add" text goalAreas backgroundColor


selectedObjective id text goalAreas =
    objectiveCard (RemoveObjective id) "Remove" text goalAreas lightBlue


objectiveCard : Msg -> String -> String -> List GoalAreaDescription -> El.Color -> Element Msg
objectiveCard buttonMsg buttonText objText goalAreas backgroundColor =
    El.el
        [ El.padding 5
        , El.height <| El.minimum 90 <| El.maximum 110 <| El.fill
        , El.width El.fill
        , El.clipY
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
