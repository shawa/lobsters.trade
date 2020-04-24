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
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import LineChart
import LineChart.Area
import LineChart.Axis
import LineChart.Axis.Intersection
import LineChart.Axis.Line
import LineChart.Axis.Range
import LineChart.Axis.Ticks
import LineChart.Axis.Title
import LineChart.Colors
import LineChart.Container
import LineChart.Coordinate
import LineChart.Dots
import LineChart.Events
import LineChart.Grid
import LineChart.Interpolation
import LineChart.Junk
import LineChart.Legends
import LineChart.Line
import List
import List.Nonempty
import Maybe
import Random
import Result
import Time
import Translations.Account
import Translations.Main


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Buy Int
    | Sell Int
    | Tick
    | SetPrice Int
    | Reset


type alias State =
    { account : Account
    , time : Int
    , price : Int
    }


type alias Model =
    List.Nonempty.Nonempty State


subscriptions : Model -> Sub Msg
subscriptions =
    always (Time.every 150 (always Tick))


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initState =
            { time = 0
            , price = 25
            , account = Account.empty |> Account.setBalance 1000
            }
    in
    ( List.Nonempty.fromElement initState
    , Cmd.none
    )


push : State -> Model -> Model
push state model =
    let
        maxHistory =
            200
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
        volatility =
            7

        lower =
            max 1 (price - volatility)

        upper =
            price + volatility
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

        Buy quantity ->
            ( model
                |> List.Nonempty.replaceHead
                    { state
                        | account =
                            state.account
                                |> Account.buy state.price quantity
                                |> Result.withDefault state.account
                    }
            , Cmd.none
            )

        Sell quantity ->
            ( model
                |> List.Nonempty.replaceHead
                    { state
                        | account =
                            state.account
                                |> Account.sell state.price quantity
                                |> Result.withDefault state.account
                    }
            , Cmd.none
            )

        Reset ->
            init ()


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
            [ viewField "Balance" (Translations.Main.currency <| Account.balance account)
            , viewField "Lobsters" (Translations.Account.lobsters account)
            ]
        ]


viewControls : Account -> Int -> Html Msg
viewControls account price =
    let
        lobsters =
            Account.lobsters account

        balance =
            Account.balance account

        buyingPower =
            balance // price
    in
    div []
        [ button [ onClick <| Buy 1 ] [ text "Buy 1" ]
        , button [ onClick <| Buy 5 ] [ text "Buy 5" ]
        , button [ onClick <| Buy 10 ] [ text "Buy 10" ]
        , button [ onClick <| Buy buyingPower ] [ text "TRAWL" ]
        , button [ onClick <| Sell lobsters ] [ text "JETTISON" ]
        , button [ onClick <| Sell 10 ] [ text "Sell 10" ]
        , button [ onClick <| Sell 5 ] [ text "Sell 5" ]
        , button [ onClick <| Sell 1 ] [ text "Sell 1" ]
        ]


type alias ChartData =
    { time : Float
    , value : Float
    }


listWith : (a -> b) -> List.Nonempty.Nonempty a -> List b
listWith f =
    List.Nonempty.map f >> List.Nonempty.toList


extractPortfolioValues : State -> ChartData
extractPortfolioValues state =
    { time = toFloat state.time
    , value = toFloat <| state.price * Account.lobsters state.account
    }


extractPotentialValues : State -> ChartData
extractPotentialValues { time, account, price } =
    { time = toFloat time
    , value =
        (price * Account.lobsters account + Account.balance account)
            |> toFloat
    }


extractBalances : State -> ChartData
extractBalances state =
    { time = toFloat state.time
    , value = toFloat <| Account.balance state.account
    }


extractPrices : State -> ChartData
extractPrices state =
    { time = toFloat state.time
    , value = toFloat state.price
    }


startAtZero : LineChart.Coordinate.Range -> LineChart.Coordinate.Range
startAtZero { max } =
    { min = 0, max = max }


viewAccountChart : Model -> Html Msg
viewAccountChart model =
    let
        balances : LineChart.Series ChartData
        balances =
            LineChart.line
                LineChart.Colors.blue
                LineChart.Dots.triangle
                "Balance"
                (listWith extractBalances model)

        portfolioValues : LineChart.Series ChartData
        portfolioValues =
            LineChart.line
                LineChart.Colors.red
                LineChart.Dots.triangle
                "Portfolio"
                (listWith extractPortfolioValues model)

        potentialValues : LineChart.Series ChartData
        potentialValues =
            LineChart.line
                LineChart.Colors.gray
                LineChart.Dots.cross
                "Potential"
                (listWith extractPotentialValues model)

        chartConfig =
            { y =
                LineChart.Axis.custom
                    { title = LineChart.Axis.Title.default "$"
                    , variable = Just << .value
                    , pixels = 400
                    , range = LineChart.Axis.Range.custom startAtZero
                    , axisLine = LineChart.Axis.Line.default
                    , ticks = LineChart.Axis.Ticks.default
                    }
            , x =
                LineChart.Axis.custom
                    { title = LineChart.Axis.Title.default "Time"
                    , variable = Just << .time
                    , pixels = 1400
                    , range = LineChart.Axis.Range.default
                    , axisLine = LineChart.Axis.Line.default
                    , ticks = LineChart.Axis.Ticks.default
                    }
            , container = LineChart.Container.default "portfolio"
            , interpolation = LineChart.Interpolation.default
            , intersection = LineChart.Axis.Intersection.default
            , legends = LineChart.Legends.default
            , events = LineChart.Events.default
            , junk = LineChart.Junk.default
            , grid = LineChart.Grid.default
            , area = LineChart.Area.default
            , line = LineChart.Line.default
            , dots = LineChart.Dots.default
            }
    in
    LineChart.viewCustom chartConfig
        [ potentialValues
        , portfolioValues
        , balances
        ]


viewPrice : Int -> Html Msg
viewPrice price =
    p [] [ text <| "1 Lobster = " ++ Translations.Main.currency price ]


viewPriceChart : Model -> Html Msg
viewPriceChart model =
    let
        balances : LineChart.Series ChartData
        balances =
            LineChart.line
                LineChart.Colors.green
                LineChart.Dots.circle
                "Price"
                (listWith extractPrices model)

        chartConfig =
            { y =
                LineChart.Axis.custom
                    { title = LineChart.Axis.Title.default "$"
                    , variable = Just << .value
                    , pixels = 250
                    , range = LineChart.Axis.Range.custom startAtZero
                    , axisLine = LineChart.Axis.Line.default
                    , ticks = LineChart.Axis.Ticks.default
                    }
            , x =
                LineChart.Axis.custom
                    { title = LineChart.Axis.Title.default "Time"
                    , variable = Just << .time
                    , pixels = 1400
                    , range = LineChart.Axis.Range.default
                    , axisLine = LineChart.Axis.Line.default
                    , ticks = LineChart.Axis.Ticks.default
                    }
            , container = LineChart.Container.default "prices"
            , interpolation = LineChart.Interpolation.default
            , intersection = LineChart.Axis.Intersection.default
            , legends = LineChart.Legends.default
            , events = LineChart.Events.default
            , junk = LineChart.Junk.default
            , grid = LineChart.Grid.default
            , area = LineChart.Area.default
            , line = LineChart.Line.default
            , dots = LineChart.Dots.default
            }
    in
    LineChart.viewCustom chartConfig
        [ balances
        ]


viewResetButton : Html Msg
viewResetButton =
    div []
        [ button [ onClick Reset ] [ text "Reset" ]
        ]


body : Model -> List (Html Msg)
body model =
    let
        state =
            List.Nonempty.head model
    in
    [ h1 [] [ text "Open Lobster Exchange \u{1F99E}" ]
    , p [] [ text "Buy and sell fresh lobsters on the open market! Use your wit to get loads of money!" ]
    , viewAccount state.account
    , viewPrice state.price
    , div [ class "window" ]
        [ viewPriceChart model
        , viewAccountChart model
        ]
    , viewControls state.account state.price
    , a [ href "https://github.com/shawa/lobsters.trade" ] [ text "source" ]
    , viewResetButton
    ]
