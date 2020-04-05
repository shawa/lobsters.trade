module Account exposing
    ( Account
    , balance
    , buy
    , empty
    , lobsters
    , sell
    , setBalance
    )

import Result exposing (Result(..))


type Account
    = Account
        { balance : Int
        , lobsters : Int
        }


setBalance : Int -> Account -> Account
setBalance amount (Account account) =
    Account { account | balance = amount }


changeBalance : Int -> Account -> Result String Account
changeBalance amount (Account account) =
    if account.balance + amount < 0 then
        Err <|
            "If I were to change the balance by "
                ++ String.fromInt amount
                ++ " it would fall below zero!"

    else
        Ok <| Account { account | balance = account.balance + amount }


changeLobsters : Int -> Account -> Result String Account
changeLobsters amount (Account account) =
    if account.lobsters + amount < 0 then
        Err <|
            "If I were to change the number of lobsters by "
                ++ String.fromInt amount
                ++ " it would fall below zero!"

    else
        Ok <| Account { account | lobsters = account.lobsters + amount }


sell : Int -> Int -> Account -> Result String Account
sell price amount account =
    account
        |> changeLobsters -amount
        |> Result.andThen (changeBalance (amount * price))


buy : Int -> Int -> Account -> Result String Account
buy price amount account =
    account
        |> changeBalance (-amount * price)
        |> Result.andThen (changeLobsters amount)


empty : Account
empty =
    Account { balance = 0, lobsters = 0 }


lobsters : Account -> Int
lobsters (Account account) =
    account.lobsters


balance : Account -> Int
balance (Account account) =
    account.balance
