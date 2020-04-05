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
import Result
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
    = Buy
    | Sell


type alias Model =
    Account


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


addCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
addCmd cmd model =
    ( model, cmd )


init : () -> ( Model, Cmd Msg )
init _ =
    Account.empty
        |> Account.setBalance 10
        |> addCmd Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Buy ->
            model
                |> Account.changeBalance -1
                |> Result.andThen (Account.changeLobsters 1)
                |> Result.withDefault model
                |> addCmd Cmd.none

        Sell ->
            model
                |> Account.changeBalance 1
                |> Result.andThen (Account.changeLobsters -1)
                |> Result.withDefault model
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
        [ p [] [ text "$1 = 1 Lobster" ]
        , button [ onClick Buy ] [ text "Buy" ]
        , button [ onClick Sell ] [ text "Sell" ]
        ]


body : Model -> List (Html Msg)
body model =
    [ h1 [] [ text "Open Lobster Exchange" ]
    , p [] [ text "Use your wit to get loads of money" ]
    , viewAccount model
    , viewControls
    ]
