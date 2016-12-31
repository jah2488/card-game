module Main exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Time exposing (Time, second)
import Common exposing (..)
import Cards exposing (..)


turnTime : Int
turnTime =
    180


main : Program Never Game Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


newCard : String -> Int -> Int -> Int -> Card
newCard name str hp cost =
    { name = name
    , maxStr = str
    , str = str
    , maxHp = hp
    , hp = hp
    , cost = cost
    }


newPlayer : String -> Player
newPlayer name =
    { name = name
    , maxHp = 50
    , hp = 50
    , wallet = 0
    }


cl : Int -> Card -> Location -> MetaCard
cl id c l =
    { id = id, card = c, loc = l, active = False, target = False }


init : ( Game, Cmd Msg )
init =
    let
        player1 =
            newPlayer "1"

        player2 =
            newPlayer "2"
    in
        ( { turn = ( 0, player1 )
          , playerOne = player1
          , playerTwo = player2
          , cards =
                [ (cl 1 bith (Hand player1))
                , (cl 11 cat (Hand player1))
                , (cl 12 dino (Hand player1))
                , (cl 13 cat (Hand player2))
                , (cl 14 dino (Hand player2))
                , (cl 2 bith (Hand player2))
                , (cl 3 bith (Board player1 One))
                , (cl 4 bith (Board player2 One))
                , (cl 5 bith (Graveyard player2))
                , (cl 6 bith (Graveyard player1))
                , (cl 7 bith (Deck player1))
                , (cl 8 bith (Deck player1))
                , (cl 9 bith (Deck player1))
                , (cl 10 bith (Deck player2))
                ]
          , time = 0
          }
        , Cmd.none
        )


updateCards : MetaCard -> MetaCard -> MetaCard
updateCards activeCard card =
    if activeCard.id == card.id then
        { card | active = True, target = False }
    else
        { card | active = False, target = False }


targetCards : MetaCard -> Game -> MetaCard -> MetaCard
targetCards activeCard game currentCard =
    case currentCard.loc of
        Board player _ ->
            if player == game.playerTwo then
                { currentCard | target = True }
            else
                { currentCard | target = False }

        Hand player ->
            { currentCard | target = False }

        Deck player ->
            { currentCard | target = False }

        Graveyard player ->
            { currentCard | target = False }


updateCard : MetaCard -> MetaCard -> Card
updateCard current target =
    let
        card =
            current.card
    in
        { card | hp = (card.hp - target.card.str) }


resolveAttack : MetaCard -> Game -> MetaCard -> MetaCard
resolveAttack targetCard game currentCard =
    let
        defaultCard =
            (cl -1 bith (Graveyard game.playerOne))

        activeCard =
            List.filter (\card -> card.active == True) game.cards
                |> List.head
                |> Maybe.withDefault defaultCard
    in
        if targetCard.id == currentCard.id then
            { currentCard
                | target = False
                , active = False
                , card = (updateCard targetCard activeCard)
            }
        else
            { currentCard
                | target = False
                , active = False
            }


playCard : MetaCard -> Game -> MetaCard -> MetaCard
playCard activeCard game currentCard =
    if activeCard.id == currentCard.id then
        { currentCard | loc = Board game.playerOne (cardsOn (Board game.playerOne One) game.cards), active = False }
    else
        currentCard


cardsOn : Location -> List MetaCard -> Placement
cardsOn location cards =
    let
        cardCount =
            List.length <| List.filter (\x -> x.loc == location) cards
    in
        case (cardCount + 1) of
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
                One


update : Msg -> Game -> ( Game, Cmd Msg )
update msg model =
    case msg of
        Active metaCard ->
            ( { model | cards = (List.map (updateCards metaCard) model.cards) }, Cmd.none )

        Attack metaCard ->
            ( { model | cards = (List.map (targetCards metaCard model) model.cards) }, Cmd.none )

        Target metaCard ->
            ( { model | cards = (List.map (resolveAttack metaCard model) model.cards) }, Cmd.none )

        PlayCard metaCard ->
            ( { model | cards = (List.map (playCard metaCard model) model.cards) }, Cmd.none )

        DrawCard ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | time = model.time + 1 }, Cmd.none )

        ClearSelection ->
            ( { model | cards = (List.map (\c -> { c | active = False }) model.cards) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Game -> Sub Msg
subscriptions model =
    Time.every second Tick


actions : Game -> MetaCard -> String -> String -> List (Svg Msg)
actions game mc x1 y1 =
    if mc.target == True then
        [ text_
            [ x x1
            , y y1
            , onClick (Target mc)
            ]
            [ text "Target"
            ]
        ]
    else if mc.active == True then
        case mc.loc of
            Board player _ ->
                if player == game.playerOne then
                    [ text_
                        [ x x1
                        , y y1
                        , onClick (Attack mc)
                        ]
                        [ text "Attack" ]
                    ]
                else
                    []

            Hand player ->
                [ text_
                    [ x x1
                    , y y1
                    , onClick (PlayCard mc)
                    ]
                    [ text "Play" ]
                ]

            Deck player ->
                [ text_
                    [ x x1
                    , y y1
                    , onClick DrawCard
                    ]
                    [ text "Draw`" ]
                ]

            Graveyard player ->
                []
    else
        []


viewCard : Game -> MetaCard -> Int -> Int -> Svg Msg
viewCard game mc posX posY =
    let
        card =
            mc.card

        cardY =
            15 * posY

        toAttr =
            \x -> (toString x) ++ "px"

        bottomY =
            toAttr (110 + cardY)

        topY =
            toAttr (25 + cardY)

        nameX =
            toAttr (posX + 20)

        costX =
            toAttr (posX + 50)

        hpX =
            toAttr (posX + 10)

        strX =
            toAttr (posX + 80)

        color =
            if mc.active == True then
                "#DDCCBB"
            else if mc.target == True then
                "#FF55AA"
            else
                "#212121"

        front =
            [ text_ [ x nameX, y bottomY, fill "#FFFFFF" ] [ text card.name ]
            , text_ [ x costX, y bottomY, fill "#55FF55" ] [ text (toString card.cost) ]
            , text_ [ x strX, y topY, fill "#5555FF" ] [ text (toString card.str) ]
            , text_ [ x hpX, y topY, fill "#FF5555" ] [ text (toString card.hp) ]
            ]
                ++ actions game mc strX (toAttr (cardY + 50))

        back =
            [ text_ [ x nameX, y bottomY, fill "#333333" ] [ text "BACK" ]
            ]

        content =
            if mc.loc == (Hand game.playerTwo) then
                back
            else
                front
    in
        g []
            [ rect
                [ x (toString posX)
                , y (toAttr cardY)
                , width "100"
                , height "125"
                , rx "15"
                , ry "15"
                , fill color
                , onClick (Active mc)
                ]
                []
            , g [] content
            ]


cardOffset : Game -> Int -> MetaCard -> Svg Msg
cardOffset game idx cl =
    let
        offset =
            250

        gutter =
            50

        y =
            case cl.loc of
                Board player _ ->
                    if player == game.playerOne then
                        20
                    else
                        10

                Hand player ->
                    if player == game.playerOne then
                        30
                    else
                        1

                Deck player ->
                    -- Offscreen
                    100

                Graveyard player ->
                    -- Offscreen
                    100

        x =
            let
                place =
                    List.filter (\x -> x.loc == cl.loc) game.cards
                        |> indexOf cl
                        |> Maybe.withDefault idx
            in
                ((gutter + ((place + 1) * offset) // 2))
    in
        viewCard game cl x y


indexOf : a -> List a -> Maybe Int
indexOf value list =
    List.indexedMap (,) list
        |> List.filter (\x -> Tuple.second (x) == value)
        |> List.map (\x -> Tuple.first (x))
        |> List.head


view : Game -> Html Msg
view model =
    svg [ viewBox "0 0 1200 800", width "1200px", height "800px" ]
        [ g [] [ text_ [ fill "#000", x "10px", y "20px" ] [ text <| toString (turnTime - model.time) ] ]
        , g [] (List.indexedMap (cardOffset model) model.cards)
        ]
