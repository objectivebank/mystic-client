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
    , searchInputEntered : Bool
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


type alias GoalAreaDescription =
    String


type GoalArea
    = StoredGoalArea UniqueID GoalAreaDescription


type alias ObjectiveDescription =
    String


type Objective
    = StoredObjective UniqueID ObjectiveDescription (List UniqueID) (List UniqueID)


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
            ( { model
                | objectiveSearchText = newText
                , matchingObjectives = interimMatching
                , searchInputEntered = searchInputEntered newText model.selectedObjectives
              }
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
            ( { model
                | selectedGoalAreas = newSelection
                , matchingObjectives = interimMatching
                , searchInputEntered = searchInputEntered model.objectiveSearchText newSelection
              }
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


searchInputEntered : String -> List UniqueID -> Bool
searchInputEntered searchText selectedGoalAreaIds =
    String.length searchText > 0 || List.length selectedGoalAreaIds > 0


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
    if searchInputEntered searchText selectedGoalAreaIds then
        currentMatchingObjectives

    else
        []


objectivesSearch : String -> String -> List UniqueID -> Cmd Msg
objectivesSearch graphqlURL searchText selectedGoalAreaIds =
    if searchInputEntered searchText selectedGoalAreaIds then
        makeRequest graphqlURL ObjectivesResponse <| objectivesQuery searchText selectedGoalAreaIds

    else
        Cmd.none


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
        StoredObjective id _ _ _ ->
            id


objectiveText : Objective -> String
objectiveText obj =
    case obj of
        StoredObjective _ text _ _ ->
            text


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
            , Input.text []
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
    El.column
        [ El.alignRight
        , El.width (El.fillPortion 10)
        , El.height El.fill
        , El.scrollbarY
        , El.paddingEach { top = 10, right = 0, bottom = 0, left = 10 }
        ]
        [ El.text <| selectedObjectivesHeading model.selectedObjectives
        , selectedView model.clientName
            (model.selectedObjectives
                |> List.map (\id -> ( id, Dict.get id model.objectives ))
                |> List.filterMap filterSecond
            )
        ]


selectedObjectivesHeading : List UniqueID -> String
selectedObjectivesHeading selectedObjectives =
    if List.length selectedObjectives == 0 then
        "Selected objectives"

    else
        "Selected objectives (" ++ String.fromInt (List.length selectedObjectives) ++ ")"


selectedView : String -> List ( UniqueID, Objective ) -> Element Msg
selectedView clientName objs =
    objectivesColumn lightBlue
        white
        (List.map
            (\( id, obj ) -> selectedObjective id <| String.replace "%1$s" clientName <| objectiveText obj)
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
            model.clientName
            model.searchInputEntered
            (model.matchingObjectives
                |> List.map (\id -> ( id, Dict.get id model.objectives ))
                |> List.filterMap filterSecond
                |> List.map (\( id, obj ) -> ( id, obj, List.member id model.selectedObjectives ))
            )
        ]


filterSecond ( a, maybeB ) =
    case maybeB of
        Just b ->
            Just ( a, b )

        Nothing ->
            Nothing


searchResults : String -> Bool -> List ( UniqueID, Objective, Bool ) -> Element Msg
searchResults clientName isSearchInputEntered foundObjectives =
    El.row
        [ El.width El.fill
        , El.height El.fill
        ]
        [ objectivesRow clientName isSearchInputEntered foundObjectives ]


objectivesRow : String -> Bool -> List ( UniqueID, Objective, Bool ) -> Element Msg
objectivesRow clientName isSearchInputEntered foundObjectives =
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
            [ heading, objectivesView clientName foundObjectives ]
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


objectivesView : String -> List ( UniqueID, Objective, Bool ) -> Element Msg
objectivesView clientName objs =
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
            (\( id, obj, selected ) ->
                let
                    objectiveBackgroundColor =
                        if selected then
                            lightBlue

                        else
                            lightGray
                in
                foundObjective id objectiveBackgroundColor <| String.replace "%1$s" clientName <| objectiveText obj
            )
            objs
        )


lightGray =
    El.rgb 0.9 0.9 0.9


white =
    El.rgb 1 1 1


lightBlue =
    El.rgb255 204 229 255


foundObjective id backgroundColor text =
    objectiveCard (AddObjective id) "Add" text backgroundColor


selectedObjective id text =
    objectiveCard (RemoveObjective id) "Remove" text lightBlue


objectiveCard : Msg -> String -> String -> El.Color -> Element Msg
objectiveCard buttonMsg buttonText objText backgroundColor =
    El.el
        [ El.padding 5
        , El.height <| El.px 90
        , El.width El.fill
        , Background.color
            backgroundColor
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
