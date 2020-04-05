module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, p, text)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Msg =
    ()


type alias Model =
    ()


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( ()
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ =
    ( ()
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body = body model
    }


body : Model -> List (Html Msg)
body _ =
    [ h1 [] [ text "Open Lobster Exchange" ]
    , p [] [ text "Use your wit to get loads of money" ]
    ]
