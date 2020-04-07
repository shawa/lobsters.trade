module Translations.Main exposing (currency)

import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)


currency : Int -> String
currency amount =
    let
        locale =
            { usLocale | decimals = Exact 0 }
    in
    "$" ++ FormatNumber.format locale (toFloat amount)
