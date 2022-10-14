module Puzzle_kadai exposing(..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Browser

main = Browser.sandbox {init=init, update=update, view=view}

type alias Model = List {x:Int, y:Int, onoff:Bool}
type alias Position = {x:Int, y:Int}
type Msg = Toggle Position

init:Model
init = [{x=1,y=1,onoff=False}
        ,{x=1,y=2,onoff=False}
        ,{x=1,y=3,onoff=False}
        ,{x=2,y=1,onoff=False}
        ,{x=2,y=2,onoff=False}
        ,{x=2,y=3,onoff=False}
        ,{x=3,y=1,onoff=False}
        ,{x=3,y=2,onoff=False}
        ,{x=3,y=3,onoff=False}]

update: Msg -> Model -> Model
update msg model =
  case msg of
    Toggle pos -> toggle model pos

toggle: Model -> Position -> Model
toggle model pos =
  List.map (\p -> if (abs(p.x-pos.x)==0 || abs(p.y-pos.y)==0)&& (abs(p.x-pos.x) <=1 && abs(p.y-pos.y) <=1) then
                      {p|onoff= not p.onoff}
                  else
                    p
            ) model


unit = 100

piece p =
      circle [cx (String.fromInt (p.x*unit))
              ,cy (String.fromInt (p.y*unit))
              ,r "50"
              ,fill (if p.onoff then
                      "pink"
                    else
                      "cyan"
                    )
              ,onClick (Toggle {x=p.x,y=p.y})]
              []

view: Model -> Html Msg
view model =
  svg [width "600"
      ,height "600"]
      (List.map piece model)
