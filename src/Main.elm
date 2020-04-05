module Main exposing (main)

import Account exposing (Account)
import Browser
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , h1
        , p
        , table
        , td
        , text
        , tr
        )
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import LineChart
import LineChart.Colors
import LineChart.Dots
import List
import List.Nonempty
import Maybe
import Random
import Result
import Time
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
    | SetPrice Int


type alias State =
    { account : Account
    , time : Int
    , price : Int
    }


type alias Model =
    List.Nonempty.Nonempty State


subscriptions : Model -> Sub Msg
subscriptions =
    always (Time.every 250 (always Tick))


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initState =
            { time = 0
            , price = 0
            , account = Account.empty |> Account.setBalance 10
            }
    in
    ( List.Nonempty.fromElement initState
    , Cmd.none
    )


push : State -> Model -> Model
push state model =
    let
        maxHistory =
            100
    in
    model
        |> List.Nonempty.cons state
        |> List.Nonempty.toList
        |> List.take maxHistory
        |> List.Nonempty.fromList
        |> Maybe.withDefault model


generatePrice : Int -> Random.Generator Int
generatePrice price =
    let
        lower =
            max 1 (price - 1)

        upper =
            min 10 (price + 1)
    in
    Random.int lower upper


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        state =
            List.Nonempty.head model
    in
    case msg of
        SetPrice newPrice ->
            ( model
                |> List.Nonempty.replaceHead
                    { state
                        | price = newPrice
                    }
            , Cmd.none
            )

        Tick ->
            ( push { state | time = state.time + 1 } model
            , Random.generate SetPrice (generatePrice state.price)
            )

        Buy ->
            ( model
                |> List.Nonempty.replaceHead
                    { state
                        | account =
                            state.account
                                |> Account.buy state.price 1
                                |> Result.withDefault state.account
                    }
            , Cmd.none
            )

        Sell ->
            ( model
                |> List.Nonempty.replaceHead
                    { state
                        | account =
                            state.account
                                |> Account.sell state.price 1
                                |> Result.withDefault state.account
                    }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Open Lobster Exchange"
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
        [ button [ onClick Buy ] [ text "Buy" ]
        , button [ onClick Sell ] [ text "Sell" ]
        ]


type alias ChartData =
    { time : Float
    , value : Float
    }


viewChart : Model -> Html Msg
viewChart model =
    let
        listWith : (a -> b) -> List.Nonempty.Nonempty a -> List b
        listWith f =
            List.Nonempty.map f >> List.Nonempty.toList

        extractPrices : State -> ChartData
        extractPrices state =
            { time = toFloat state.time
            , value = toFloat state.price
            }

        extractBalances : State -> ChartData
        extractBalances state =
            { time = toFloat state.time
            , value = toFloat <| Account.balance state.account
            }

        balances : LineChart.Series ChartData
        balances =
            LineChart.line
                LineChart.Colors.green
                LineChart.Dots.triangle
                "Balance"
                (listWith extractBalances model)

        prices : LineChart.Series ChartData
        prices =
            LineChart.line
                LineChart.Colors.pink
                LineChart.Dots.circle
                "Price"
                (listWith extractPrices model)
    in
    LineChart.view .time
        .value
        [ prices
        , balances
        ]


body : Model -> List (Html Msg)
body model =
    [ h1 [] [ text "Open Lobster Exchange \u{1F99E}" ]
    , p [] [ text "Buy and sell fresh lobsters on the open market! Use your wit to get loads of money!" ]
    , viewAccount (List.Nonempty.head model).account
    , viewControls
    , viewChart model
    , a [ href "https://github.com/shawa/lobsters.trade" ] [ text "source" ]
    ]
