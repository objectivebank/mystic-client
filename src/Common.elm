module Common exposing (..)


filterSecond ( a, maybeB ) =
    case maybeB of
        Just b ->
            Just ( a, b )

        Nothing ->
            Nothing
