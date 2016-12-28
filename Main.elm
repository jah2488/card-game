module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Time exposing (Time, second)
import Common exposing (..)
import Cards exposing (..)


main : Program Never
main =
    App.program
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


type alias Game =
    { turn : ( Int, Player )
    , playerOne : Player
    , playerTwo : Player
    , cards : List MetaCard
    }


cl : Int -> Card -> Location -> MetaCard
cl id c l =
    { id = id, card = c, loc = l, active = False }


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
                [ (cl 1 b (Hand player1))
                , (cl 2 b (Hand player2))
                , (cl 3 b (Board player1))
                , (cl 4 b (Board player2))
                , (cl 5 b (Graveyard player2))
                , (cl 6 b (Graveyard player1))
                , (cl 7 b (Deck player1))
                , (cl 8 b (Deck player1))
                , (cl 9 b (Deck player1))
                , (cl 10 b (Deck player2))
                ]
          }
        , Cmd.none
        )


type Msg
    = NoOp
    | Active MetaCard
    | PlayCard Card
    | Tick Time


updateCards : MetaCard -> MetaCard -> MetaCard
updateCards activeCard card =
    if activeCard.id == card.id then
        { card | active = True }
    else
        { card | active = False }


update : Msg -> Game -> ( Game, Cmd Msg )
update msg model =
    case msg of
        Active metaCard ->
            ( { model | cards = (List.map (updateCards metaCard) model.cards) }, Cmd.none )

        PlayCard card ->
            ( model, Cmd.none )

        Tick newTime ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Game -> Sub Msg
subscriptions model =
    Time.every second Tick


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
                "#FFAAFF"
            else
                "#111111"

        front =
            [ text_ [ x nameX, y bottomY, fill "#FFFFFF" ] [ text card.name ]
            , text_ [ x costX, y bottomY, fill "#FFFFFF" ] [ text (toString card.cost) ]
            , text_ [ x strX, y topY, fill "#FFFFFF" ] [ text (toString card.maxStr) ]
            , text_ [ x hpX, y topY, fill "#FFFFFF" ] [ text (toString card.maxHp) ]
            ]

        back =
            [ text_ [ x nameX, y bottomY, fill "#FFFFFF" ] [ text "BACK" ]
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
            150

        gutter =
            50

        y =
            case cl.loc of
                Board player ->
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
    in
        viewCard game cl ((gutter + ((idx + 1) * offset))) y


view : Game -> Html Msg
view model =
    svg [ viewBox "0 0 800 600", width "800px", height "600px" ]
        [ text (toString model)
        , g [] (List.indexedMap (cardOffset model) model.cards)
        ]
