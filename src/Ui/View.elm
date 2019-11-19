module Ui.View exposing (..)

import Browser exposing (Document)
import Clippy exposing (clippy)
import Common exposing (filterSecond)
import Data.Types exposing (ClientName, ClientPronouns(..), Flags, GoalArea, GoalAreaDescription, Model, Msg(..), ObjectiveCardData, UniqueID, goalAreaText, makeObjectiveCardData, objectiveText, selectedObjectiveData)
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
        -- [ El.layout [] <| applicationView model
        [ El.layout [] <| newView model
        ]
    }


newView : Model -> Element Msg
newView model =
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
        ]
        [ El.row
            [ El.width El.fill
            , El.height <| El.maximum 800 <| El.minimum 650 <| El.fill
            ]
            [ El.column
                [ El.width <| El.fillPortion 3
                , El.height El.fill
                , El.padding panelPadding
                , El.spacing panelSpacing
                , Background.color <| El.rgb255 186 85 211
                ]
                [ El.row
                    [ El.width El.fill
                    , El.height <| El.px 30
                    , Background.color <| El.rgb255 216 191 216
                    ]
                    [ objectiveSearchLabelView
                    ]
                , El.row
                    [ El.width El.fill
                    , El.height <| El.fillPortion 2
                    , Background.color <| El.rgb255 216 191 216
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
                    , Background.color <| El.rgb255 216 191 216
                    ]
                    [ goalAreasView2 model ]
                , El.row
                    [ El.width El.fill
                    , El.height <| El.fillPortion 6
                    , Background.color <| El.rgb255 216 191 216
                    ]
                    [ clientVariablesView2 model ]
                ]
            , El.column
                [ El.width <| El.fillPortion 5
                , El.height El.fill
                , El.padding panelPadding
                , El.spacing panelSpacing
                , Background.color <| El.rgb255 230 230 250
                ]
                [ El.row
                    [ El.width El.fill
                    , El.height <| El.px 30
                    , El.spaceEvenly
                    , Background.color <| El.rgb255 216 191 216
                    ]
                    (selectedObjectivesCopyView model selectedObjectivesCardData)
                , El.row
                    [ El.width El.fill
                    , El.height El.fill
                    , Background.color <| El.rgb255 216 191 216
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
            , Background.color <| El.rgb255 128 0 128
            ]
            [ El.column
                [ El.width El.fill
                , El.height El.fill
                , El.padding panelPadding
                , El.spacing panelSpacing
                ]
                [ foundObjectivesHeading model.searchInputEntered
                , searchResultsView2 <| makeObjectiveCardData model
                ]
            ]
        ]


panelPadding =
    8


panelSpacing =
    2


objectiveSearchLabelView =
    El.text "Objective Search"


objectiveSearchBarView currentSearchText =
    Input.text [ El.width El.fill ]
        { onChange = \text -> SearchTextEntered text
        , text = currentSearchText
        , placeholder = Just (Input.placeholder [] <| El.text "Search...")
        , label = Input.labelHidden "Enter objective search terms"
        }


goalAreasView2 : Model -> Element Msg
goalAreasView2 model =
    El.column
        []
        [ El.text "Goal Areas"
        , goalAreaCheckboxes model.goalAreas model.selectedGoalAreas
        ]


clientVariablesView2 : Model -> Element Msg
clientVariablesView2 model =
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


selectedObjectivesView : List ObjectiveCardData -> Element Msg
selectedObjectivesView objs =
    objectivesColumn2
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


searchResultsView2 : List ObjectiveCardData -> Element Msg
searchResultsView2 foundObjectives =
    El.row
        [ El.spaceEvenly
        , El.width El.fill
        , El.height El.fill
        ]
        [ El.column [ El.width El.fill, El.spacing 10 ]
            [ objectivesView2 foundObjectives ]
        ]


objectivesView2 : List ObjectiveCardData -> Element Msg
objectivesView2 objs =
    objectivesColumn2
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


objectivesColumn2 : El.Color -> List (Element msg) -> Element msg
objectivesColumn2 backgroundColor elements =
    El.column
        [ Background.color backgroundColor
        , El.alignTop
        , El.width El.fill
        , El.height El.fill
        , El.spacing 8
        ]
        elements


card : Element Msg
card =
    El.row
        [ El.width El.fill
        , El.height <| El.minimum 100 <| El.maximum 120 <| El.fill
        , Background.color <| El.rgb255 238 130 238
        ]
        []


applicationView : Model -> Element Msg
applicationView model =
    El.column
        [ El.width El.fill
        , El.height El.fill
        , El.padding 10
        ]
        [ searchBarView model.objectiveSearchText
        , middleView model
        , searchResultsView model.searchInputEntered <| makeObjectiveCardData model
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
        , El.height <| El.px 600
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
                |> List.map (selectedObjectiveData model.clientName model.clientPronouns model.goalAreas)
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
        , Attributes.style "background-color" "unset"
        , Attributes.style "border-width" "0"
        ]
        [ clippy "copy-button-image" ]
        |> El.html


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
    El.column [ El.height El.fill, El.spacing 30 ]
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
