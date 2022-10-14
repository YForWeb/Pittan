module Drag exposing (..)

import Browser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Html exposing (Html)
import Json.Decode as JD

main = Browser.element{init=init
                      ,update=update
                      ,view=view
                      ,subscriptions=subscriptions}

type alias Model = {mouse:Mouse}
type alias Mouse = {x:Float, y:Float}
type Msg = MouseMoved Float Float

init: () -> (Model, Cmd Msg)
init _ = ({mouse={x=0,y=0}}
          , Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseMoved x y ->  ({model|mouse=Debug.log "move"{x=x,y=y}}, Cmd.none)

view: Model -> Html Msg
view model =
  Html.div []
      [svg [width "400"
            ,height "400"
            ,onMouseMove MouseMoved]
            [
            circle [cx (String.fromFloat model.mouse.x)
                   ,cy (String.fromFloat model.mouse.y)
                   ,r "30"
                   ,fill "brown"
                   ,stroke "black"][]
            ]
      ]

onMouseMove msg =
  on "mousemove"
    (JD.map2 msg
      (JD.field "offsetX" JD.float)
      (JD.field "offsetY" JD.float)
    )

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.none
