module Common exposing (..)


type alias Card =
    { name : String
    , maxHp : Int
    , hp : Int
    , maxStr : Int
    , str : Int
    , cost : Int
    }


type alias Player =
    { name : String
    , maxHp : Int
    , hp : Int
    , wallet : Int
    , hand : List Card
    , deck : List Card
    , graveyard : List Card
    }


type alias Board =
    { cards : ( List ( Card, Player ), List ( Card, Player ) )
    }
