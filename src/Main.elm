module Main exposing (main)

import API.Object
import API.Object.CategorizedObjectiveType as CategorizedObjectiveType
import API.Object.GoalAreaType as GoalAreaType
import API.Query as Query
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Url exposing (Url)


type alias Model =
    { objectives : Dict UniqueID Objective
    , objectiveSearchText : String
    , matchingObjectives : List UniqueID
    , selectedObjectives : List UniqueID
    , goalAreas : Dict UniqueID GoalArea
    , selectedGoalAreas : List UniqueID
    , clientName : String
    , graphqlURL : String
    }


type Msg
    = AddObjective UniqueID
    | RemoveObjective UniqueID
    | UrlRequest
    | SearchTextEntered String
    | GoalAreaToggled UniqueID Bool
    | ClientNameUpdated String
    | GoalAreasResponse (Result (Graphql.Http.Error (List GoalArea)) (List GoalArea))
    | ObjectivesResponse (Result (Graphql.Http.Error (List Objective)) (List Objective))


type alias Flags =
    { graphqlURL : String }


type alias UniqueID =
    Int


type GoalArea
    = StoredGoalArea UniqueID String


type Objective
    = StoredObjective UniqueID String


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
      }
    , makeRequest flags.graphqlURL GoalAreasResponse goalAreasQuery
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
            ( { model | objectiveSearchText = newText, matchingObjectives = interimMatching }
            , objectivesSearch model.graphqlURL newText model.selectedGoalAreas
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
            ( { model | selectedGoalAreas = newSelection, matchingObjectives = interimMatching }
            , objectivesSearch model.graphqlURL model.objectiveSearchText newSelection
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


goalAreasQuery : SelectionSet (List GoalArea) RootQuery
goalAreasQuery =
    Query.goalAreas goalAreaSelection


goalAreaSelection : SelectionSet GoalArea API.Object.GoalAreaType
goalAreaSelection =
    SelectionSet.map2 StoredGoalArea
        GoalAreaType.id
        GoalAreaType.description


matchingForSearchInput : List UniqueID -> String -> List UniqueID -> List UniqueID
matchingForSearchInput currentMatchingObjectives searchText selectedGoalAreaIds =
    if String.length searchText == 0 && List.length selectedGoalAreaIds == 0 then
        []

    else
        currentMatchingObjectives


objectivesSearch : String -> String -> List UniqueID -> Cmd Msg
objectivesSearch graphqlURL searchText selectedGoalAreaIds =
    if String.length searchText == 0 && List.length selectedGoalAreaIds == 0 then
        Cmd.none

    else
        makeRequest graphqlURL ObjectivesResponse <| objectivesQuery searchText selectedGoalAreaIds


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
    SelectionSet.map2 StoredObjective
        CategorizedObjectiveType.id
        CategorizedObjectiveType.description


makeRequest : String -> (Result (Graphql.Http.Error decodesTo) decodesTo -> Msg) -> SelectionSet decodesTo RootQuery -> Cmd Msg
makeRequest graphqlUrl msgFunction selectionsSet =
    Graphql.Http.send msgFunction <| Graphql.Http.queryRequest graphqlUrl selectionsSet


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
        StoredObjective id _ ->
            id


objectiveText : Objective -> String
objectiveText obj =
    case obj of
        StoredObjective _ text ->
            text


applicationView : Model -> Element Msg
applicationView model =
    El.column
        [ El.width El.fill
        , El.height El.fill
        , El.padding 10
        , El.spacing 10
        ]
        [ searchBarView model.objectiveSearchText
        , goalAreasView model
        , searchResultsView model
        ]


searchBarView : String -> Element Msg
searchBarView currentSearchText =
    El.row
        [ El.width El.fill ]
        [ El.column [ El.width El.fill, El.spacing 8 ]
            [ El.text "Objective Search"
            , Input.text []
                { onChange = \text -> SearchTextEntered text
                , text = currentSearchText
                , placeholder = Just (Input.placeholder [] <| El.text "Search...")
                , label = Input.labelHidden "Enter objective search terms"
                }
            ]
        ]


goalAreasView : Model -> Element Msg
goalAreasView model =
    El.row
        []
        [ El.column []
            (El.text "Goal Areas"
                :: goalAreaCheckboxes model.goalAreas model.selectedGoalAreas
            )
        ]


goalAreaCheckboxes : Dict UniqueID GoalArea -> List UniqueID -> List (Element Msg)
goalAreaCheckboxes goalAreas selectedGoalAreas =
    Dict.toList goalAreas
        |> List.map (\( id, ga ) -> goalAreaCheckbox id (List.member id selectedGoalAreas) (goalAreaText ga))


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
    El.column [ El.width El.fill, El.spacing 24 ]
        [ clientVariablesView model
        , searchResults
            model.clientName
            (model.matchingObjectives
                |> List.map (\id -> ( id, Dict.get id model.objectives ))
                |> List.filterMap filterSecond
            )
            (model.selectedObjectives
                |> List.map (\id -> ( id, Dict.get id model.objectives ))
                |> List.filterMap filterSecond
            )
        ]


filterSecond ( a, maybeB ) =
    case maybeB of
        Just b ->
            Just ( a, b )

        Nothing ->
            Nothing


searchResults : String -> List ( UniqueID, Objective ) -> List ( UniqueID, Objective ) -> Element Msg
searchResults clientName foundObjectives selectedObjectives =
    El.row
        [ El.width El.fill
        , El.height El.fill
        ]
        [ objectivesRow clientName foundObjectives selectedObjectives ]


objectivesRow : String -> List ( UniqueID, Objective ) -> List ( UniqueID, Objective ) -> Element Msg
objectivesRow clientName foundObjectives selectedObjectives =
    El.row
        [ El.spaceEvenly
        , El.width El.fill
        , El.height El.fill
        ]
        [ objectivesView clientName foundObjectives, centerGap, selectedView clientName selectedObjectives ]


objectivesColumn : El.Color -> List (Element msg) -> Element msg
objectivesColumn borderColor elements =
    El.column
        [ Border.color borderColor
        , Border.width 1
        , El.alignTop
        , El.width (El.fillPortion 8)
        , El.height El.fill
        , El.padding 8
        , El.spacing 8
        ]
        elements


objectivesView : String -> List ( UniqueID, Objective ) -> Element Msg
objectivesView clientName objs =
    objectivesColumn lightGray
        (List.map
            (\( id, obj ) -> foundObjective id <| String.replace "%1$s" clientName <| objectiveText obj)
            objs
        )


lightGray =
    El.rgb 0.9 0.9 0.9


selectedView : String -> List ( UniqueID, Objective ) -> Element Msg
selectedView clientName objs =
    objectivesColumn lightGray
        (List.map
            (\( id, obj ) -> selectedObjective id <| String.replace "%1$s" clientName <| objectiveText obj)
            objs
        )


foundObjective id text =
    objectiveCard (AddObjective id) "Add" text


selectedObjective id text =
    objectiveCard (RemoveObjective id) "Remove" text


objectiveCard : Msg -> String -> String -> Element Msg
objectiveCard buttonMsg buttonText objText =
    El.el
        [ El.padding 5
        , El.height <| El.px 90
        , El.width El.fill
        , Background.color
            lightGray
        ]
        (El.paragraph [] [ El.text objText, El.el [ El.alignRight ] (objectiveButton buttonMsg buttonText) ])


objectiveButton : Msg -> String -> Element Msg
objectiveButton msg buttonText =
    Input.button
        [ Background.color <| El.rgb 238 238 238
        ]
        { onPress = Just msg
        , label = El.text buttonText
        }


centerGap : Element msg
centerGap =
    El.column [ El.width (El.fillPortion 1) ] []
