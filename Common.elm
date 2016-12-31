module Common exposing (..)

import Time exposing (Time, second)


type Msg
    = NoOp
    | Active MetaCard
    | DrawCard
    | PlayCard MetaCard
    | Attack MetaCard
    | Target MetaCard
    | Tick Time
    | ClearSelection


type Placement
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine


type Location
    = Board Player Placement
    | Hand Player
    | Deck Player
    | Graveyard Player


type alias Game =
    { turn : ( Int, Player )
    , playerOne : Player
    , playerTwo : Player
    , cards : List MetaCard
    , time : Int
    }


type alias Pos =
    { x : Int, y : Int }


type alias MetaCard =
    { id : Int, card : Card, active : Bool, target : Bool, loc : Location }


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
