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
    | OutOfPlay


type alias Player =
    { name : String
    , maxHp : Int
    , hp : Int
    , wallet : Int
    }


type Location
    = Board Player
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
    { id : Int, card : Card, active : Bool, target : Bool, loc : Location, placement : Placement }


type alias Card =
    { name : String
    , maxHp : Int
    , hp : Int
    , maxStr : Int
    , str : Int
    , cost : Int
    }


intToPlacement : Int -> Placement
intToPlacement n =
    case n of
        1 ->
            One

        2 ->
            Two

        3 ->
            Three

        4 ->
            Four

        5 ->
            Five

        6 ->
            Six

        7 ->
            Seven

        8 ->
            Eight

        9 ->
            Nine

        _ ->
            OutOfPlay


placementToInt : Placement -> Int
placementToInt placement =
    case placement of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        _ ->
            0
