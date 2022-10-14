module TimerTest exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Time
import Random
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)

main = Browser.element {init=init
                        ,update = update
                        ,view = view
                        ,subscriptions=subscriptions}

type alias Model = {config:Config
                    ,elapsedTime: Int}
type alias Config = List Piece
type alias Piece = {x:Int, y:Int}

type Msg = Elapsed Time.Posix | NewPiece Int | Move KeyboardEvent

init: () -> (Model, Cmd Msg)
init _ =
  ({config=[{x=0,y=0}], elapsedTime=0}
  ,Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model=
  case msg of
    Elapsed pt ->  (down model
                    ,Random.generate NewPiece (Random.int 0 5))
    NewPiece x -> ({model| config={x=x,y=0}::model.config}
                    ,Cmd.none)
    Move keydata ->
      let
          key = keydata.key
      in
        case key of
          Just k ->
            if k=="ArrowRight" then
              (right model, Cmd.none)
            else if k=="ArrowLeft" then
              (left model, Cmd.none)
            else
              (model, Cmd.none)
          _ -> (model, Cmd.none)

right: Model -> Model
right model =
  {model | config = List.map (\p -> {p|x=p.x+1}) model.config}

left: Model -> Model
left model =
  {model | config = List.map (\p -> {p|x=p.x-1}) model.config}


down: Model -> Model
down model =
  {model | elapsedTime = model.elapsedTime+1
          ,config = List.map (\p -> {p|y=p.y+1}) model.config}

unit=50
pieceView: Piece -> Svg Msg
pieceView p =
  rect [x (String.fromInt (unit*p.x))
        ,y (String.fromInt (unit*p.y))
        ,width (String.fromInt unit)
        ,height (String.fromInt unit)
        ,fill "green"
        ][]

configView: Config -> List (Svg Msg)
configView config =
  List.map pieceView config

view: Model -> Html Msg
view model =
  Html.div []
      [
        svg [width "300"
            ,height "400"
            ]
            (configView model.config)
        ,Html.div[]
            [Html.text (String.fromInt model.elapsedTime)]
      ]

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch [
    Time.every 1000 Elapsed
    ,onKeyDown (Json.map Move decodeKeyboardEvent)
  ]
