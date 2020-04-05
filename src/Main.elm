module Main exposing (main)

import Account exposing (Account)
import Browser
import Html
    exposing
        ( Html
        , button
        , div
        , h1
        , p
        , table
        , td
        , text
        , tr
        )
import Html.Events exposing (onClick)
import Translations.Account


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Increment
    | Decrement


type alias Model =
    Account


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( Account.empty
    , Cmd.none
    )


addCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
addCmd cmd model =
    ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            model
                |> Account.changeBalance 1
                |> addCmd Cmd.none

        Decrement ->
            model
                |> Account.changeBalance -1
                |> addCmd Cmd.none


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body = body model
    }


viewAccount : Model -> Html Msg
viewAccount model =
    let
        cell x =
            td [] [ text x ]

        viewField key value =
            tr [] <| List.map cell [ key, value ]
    in
    div []
        [ table []
            [ viewField "Balance" (Translations.Account.balance model)
            , viewField "Lobsters" (Translations.Account.lobsters model)
            ]
        ]


viewControls : Html Msg
viewControls =
    div []
        [ button [ onClick Increment ] [ text "+" ]
        , button [ onClick Decrement ] [ text "-" ]
        ]


body : Model -> List (Html Msg)
body model =
    [ h1 [] [ text "Open Lobster Exchange" ]
    , p [] [ text "Use your wit to get loads of money" ]
    , viewAccount model
    , viewControls
    ]
