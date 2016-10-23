module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
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


newBoard : Player -> Player -> Board
newBoard player1 player2 =
    { cards = ( [], [] )
    }


newPlayer : String -> Player
newPlayer name =
    { name = name
    , maxHp = 50
    , hp = 50
    , wallet = 0
    , hand = []
    , deck = [ b, b, b, b ]
    , graveyard = []
    }


type alias Game =
    { turn : ( Int, Player )
    , board : Board
    , playerOne : Player
    , playerTwo : Player
    }


init : ( Game, Cmd Msg )
init =
    let
        player1 =
            newPlayer "1"

        player2 =
            newPlayer "2"
    in
        ( { turn = ( 0, player1 )
          , board = newBoard player1 player2
          , playerOne = player1
          , playerTwo = player2
          }
        , Cmd.none
        )


type Msg
    = NoOp
    | PlayCard Card
    | Tick Time


update : Msg -> Game -> ( Game, Cmd Msg )
update msg model =
    case msg of
        PlayCard card ->
            ( model, Cmd.none )

        Tick newTime ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Game -> Sub Msg
subscriptions model =
    Time.every second Tick


view : Game -> Html Msg
view model =
    svg [ viewBox "0 0 800 600", width "800px", height "600px" ]
        [ circle [ cx "50", cy "50", r "55", fill "#0B79CE" ] []
        , line [ x1 "50", y1 "50", x2 "10", y2 "20", stroke "#023963" ] []
        , text (toString model)
        ]
