module Main exposing (main)

import Browser exposing (Document)
import Html exposing (text)


type Model
    = Q


type Msg
    = X


type alias Flags =
    Int


init : flags -> ( Model, Cmd msg )
init flags =
    ( Q, Cmd.none )


view : model -> Document msg
view model =
    { title = "Objective Bank", body = [ text "Staging Area" ] }


update : msg -> model -> ( model, Cmd msg )
update msg model =
    ( model, Cmd.none )


subscriptions : model -> Sub msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }
