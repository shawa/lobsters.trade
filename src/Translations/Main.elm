module Translations.Main exposing (price)

import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)


price : Int -> String
price p =
    let
        locale =
            { usLocale | decimals = Exact 0 }
    in
    "1 Lobster = $" ++ FormatNumber.format locale (toFloat p)
