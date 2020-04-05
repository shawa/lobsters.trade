module Account exposing
    ( Account
    , balance
    , changeBalance
    , changeLobsters
    , empty
    , lobsters
    )


type Account
    = Account
        { balance : Int
        , lobsters : Int
        }


changeBalance : Int -> Account -> Account
changeBalance amount (Account account) =
    Account { account | balance = account.balance + amount }


changeLobsters : Int -> Account -> Account
changeLobsters amount (Account account) =
    Account { account | lobsters = account.lobsters + amount }


empty : Account
empty =
    Account { balance = 0, lobsters = 0 }


lobsters : Account -> Int
lobsters (Account account) =
    account.lobsters


balance : Account -> Int
balance (Account account) =
    account.balance
