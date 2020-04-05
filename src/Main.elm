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
import LineChart
import List
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
    | Tick


type alias Model =
    { account : Account
    , time : Int
    , price : Int
    }


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


addCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
addCmd cmd model =
    ( model, cmd )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , price = 0
      , account = Account.empty |> Account.setBalance 10
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | time = model.time + 1 }
            , Cmd.none
            )

        Buy ->
            ( { model
                | account =
                    model.account
                        |> Account.changeBalance -1
                        |> Result.andThen (Account.changeLobsters 1)
                        |> Result.withDefault model.account
              }
            , Cmd.none
            )

        Sell ->
            ( { model
                | account =
                    model.account
                        |> Account.changeBalance 1
                        |> Result.andThen (Account.changeLobsters -1)
                        |> Result.withDefault model.account
              }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body = body model
    }


viewAccount : Account -> Html Msg
viewAccount account =
    let
        cell x =
            td [] [ text x ]

        viewField key value =
            tr [] <| List.map cell [ key, value ]
    in
    div []
        [ table []
            [ viewField "Balance" (Translations.Account.balance account)
            , viewField "Lobsters" (Translations.Account.lobsters account)
            ]
        ]


viewControls : Html Msg
viewControls =
    div []
        [ p [] [ text "$1 = 1 Lobster" ]
        , button [ onClick Buy ] [ text "Buy" ]
        , button [ onClick Sell ] [ text "Sell" ]
        ]


type alias ChartData =
    { time : Int
    , price : Int
    }


viewChart : Model -> Html Msg
viewChart _ =
    let
        initData : List ChartData
        initData =
            List.range 0 100
                |> List.indexedMap
                    (\time price ->
                        { time = time
                        , price = time * 2
                        }
                    )
    in
    LineChart.view1 (.price >> toFloat)
        (.time >> toFloat)
        initData


body : Model -> List (Html Msg)
body model =
    [ h1 [] [ text "Open Lobster Exchange" ]
    , p [] [ text "Use your wit to get loads of money" ]
    , viewAccount model.account
    , viewControls
    , viewChart model
    ]
