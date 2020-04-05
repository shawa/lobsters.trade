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
import List.Nonempty as NE
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
    NE.Nonempty State


subscriptions : Model -> Sub Msg
subscriptions =
    always (Time.every 250 (always Tick))


addCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
addCmd cmd model =
    ( model, cmd )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initState =
            { time = 0
            , price = 0
            , account = Account.empty |> Account.setBalance 10
            }
    in
    ( NE.fromElement initState
    , Cmd.none
    )


getState : Model -> State
getState =
    NE.head


push : State -> Model -> Model
push state model =
    let
        newState =
            NE.cons state model
    in
    if NE.length newState > 100 then
        newState |> NE.reverse |> NE.pop |> NE.reverse

    else
        newState


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
            getState model
    in
    case msg of
        SetPrice newPrice ->
            ( model
                |> NE.replaceHead
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
                |> NE.replaceHead
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
                |> NE.replaceHead
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
        listWith : (a -> b) -> NE.Nonempty a -> List b
        listWith f =
            NE.map f >> NE.toList

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
    , viewAccount (getState model).account
    , viewControls
    , viewChart model
    , a [ href "https://github.com/shawa/lobsters.trade" ] [ text "source" ]
    ]
