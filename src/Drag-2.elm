module Drag exposing (..)

import Browser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Html exposing (Html)
import Json.Decode as JD
import Browser.Events

main = Browser.element{init=init
                      ,update=update
                      ,view=view
                      ,subscriptions=subscriptions}

type alias Peg = {id:Int, x:Int, y:Int, moving:Bool}
type alias Config = List Peg
type alias Model = {config:Config, mouse:Mouse}
type alias Mouse = {x:Float, y:Float}
type Msg = MouseMoved Float Float
          |DragStart Peg
          |DragEnd

init: () -> (Model, Cmd Msg)
init _ = ({mouse = {x=0,y=0}
          ,config = [Peg 0 1 1 False
                    ,Peg 1 2 3 False]}
          , Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseMoved x y ->  ({model|mouse=Debug.log "move"{x=x,y=y}}, Cmd.none)
    DragStart peg -> ({model|config=List.map
                      (\p-> if p.id==peg.id then
                              Debug.log "down" {p|moving=True}
                            else
                              p) model.config}
                      ,Cmd.none)
    DragEnd -> ({model| config=List.map
                                (\p-> if p.moving then
                                        {p|moving=False
                                          ,x= floor (model.mouse.x/unit)
                                          ,y= floor (model.mouse.y/unit)}
                                      else
                                        p) model.config}
                ,Cmd.none)
unit=50
pegView: Peg -> Svg Msg
pegView peg =
  circle [cx (String.fromInt (unit*peg.x))
          ,cy (String.fromInt (unit*peg.y))
          ,r (String.fromInt (unit//2))
          ,fill "brown"
          ,stroke "black"
          ,onMouseDown (DragStart peg)]
          []

movingView: Float -> Float -> Svg Msg
movingView x y =
  circle [cx (String.fromFloat x)
          ,cy (String.fromFloat y)
          ,r (String.fromInt (unit//2))
          ,fill "brown"
          ,stroke "black"
          ]
          []


view: Model -> Html Msg
view model =
  Html.div []
      [svg [width "400"
            ,height "400"
            ,onMouseMove MouseMoved]
            (List.map (\p -> if p.moving then
                                movingView model.mouse.x model.mouse.y
                             else
                               pegView p)
             model.config)
      ]

onMouseMove msg =
  on "mousemove"
    (JD.map2 msg
      (JD.field "offsetX" JD.float)
      (JD.field "offsetY" JD.float)
    )

subscriptions: Model -> Sub Msg
subscriptions model =
  Browser.Events.onMouseUp (JD.succeed DragEnd)
