module Account exposing
    ( Account
    , balance
    , changeBalance
    , changeLobsters
    , empty
    , lobsters
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
        Err <| "Insufficent balance to add " ++ String.fromInt amount

    else
        Ok <| Account { account | balance = account.balance + amount }


changeLobsters : Int -> Account -> Result String Account
changeLobsters amount (Account account) =
    if account.lobsters + amount < 0 then
        Err <| "Insufficent lobsters to add " ++ String.fromInt amount

    else
        Ok <| Account { account | lobsters = account.lobsters + amount }


empty : Account
empty =
    Account { balance = 0, lobsters = 0 }


lobsters : Account -> Int
lobsters (Account account) =
    account.lobsters


balance : Account -> Int
balance (Account account) =
    account.balance
