module Translations.Account exposing (balance, lobsters)

import Account exposing (Account)


balance : Account -> String
balance account =
    "$"
        ++ String.fromInt
            (Account.balance account)


lobsters : Account -> String
lobsters account =
    account
        |> Account.lobsters
        |> String.fromInt
