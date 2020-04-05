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
import List.Nonempty as NE
import Random
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
    always Sub.none


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
        delta =
            15

        lower =
            max (price - delta) 1

        upper =
            price + delta
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
            ( model
                |> NE.cons { state | time = state.time + 1 }
            , Random.generate SetPrice (generatePrice state.price)
            )

        Buy ->
            ( model
                |> NE.replaceHead
                    { state
                        | account =
                            state.account
                                |> Account.changeBalance -1
                                |> Result.andThen (Account.changeLobsters 1)
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
                                |> Account.changeBalance 1
                                |> Result.andThen (Account.changeLobsters -1)
                                |> Result.withDefault state.account
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
    , value : Int
    }


viewChart : Model -> Html Msg
viewChart model =
    let
        prices : State -> ChartData
        prices state =
            { time = state.time
            , value = state.price
            }

        balances : State -> ChartData
        balances state =
            { time = state.time
            , value = Account.balance state.account
            }

        listWith : (a -> b) -> NE.Nonempty a -> List b
        listWith f nonEmpty =
            nonEmpty
                |> NE.map f
                |> NE.toList
    in
    LineChart.view2
        (.time >> toFloat)
        (.value >> toFloat)
        (model |> listWith prices)
        (model |> listWith balances)


body : Model -> List (Html Msg)
body model =
    [ h1 [] [ text "Open Lobster Exchange" ]
    , p [] [ text "Use your wit to get loads of money" ]
    , viewAccount (getState model).account
    , viewControls
    , viewChart model
    , button [ onClick Tick ] [ text "Tick" ]
    ]
