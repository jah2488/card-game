module Cards exposing (..)

import Common exposing (..)


b : Card
b =
    blank "fizz" 1 1 1


blank : String -> Int -> Int -> Int -> Card
blank name str hp cost =
    { name = name
    , str = str
    , maxStr = str
    , hp = hp
    , maxHp = hp
    , cost = cost
    }
