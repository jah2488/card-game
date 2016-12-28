module Common exposing (..)


type Location
    = Board Player
    | Hand Player
    | Deck Player
    | Graveyard Player


type alias Pos =
    { x : Int, y : Int }


type alias MetaCard =
    { id : Int, card : Card, active : Bool, loc : Location }


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
    }
