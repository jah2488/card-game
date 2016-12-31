module Cards exposing (..)

import Common exposing (..)


bith : Card
bith =
    blank "bith" 1 1 1


cat : Card
cat =
    blank "cat" 1 2 1


dino : Card
dino =
    blank "dino" 2 1 1


blank : String -> Int -> Int -> Int -> Card
blank name str hp cost =
    { name = name
    , str = str
    , maxStr = str
    , hp = hp
    , maxHp = hp
    , cost = cost
    }
