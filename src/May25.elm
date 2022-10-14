module May25 exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Html exposing (Html)
import Browser

main = Browser.sandbox{ init = init, view = view, update = update }

type alias Model = {config: Config, space: Piece}
type alias Config = List Piece
type alias Piece = {x:Int, y:Int, n:Int} --pieceに書いてある数字
 --alias:元々ある型(Int)をPieceという新しい型にする
type Msg = Slide Piece --Slideは既存の型ではない

size = 4
init: Model --initで初期化
init = {config = Debug.log "" startConfig, space={x=size-1, y=size-1, n=0}} --右隅 -- 0,1,..., size-1
startConfig: Config
startConfig =
  List.map (\i -> {x=modBy size i, y=i//size, n=i+1 })
  (List.range 0 (size^2-2))

update: Msg -> Model -> Model --updateで更新
update msg model =
  case msg of
    Slide piece -> slide piece model

adjacent: Piece -> Piece -> Bool
adjacent p q =
  ((abs(p.x-q.x)) + (abs(p.y-q.y))) == 1

slide: Piece -> Model -> Model
slide piece model =
  if adjacent piece model.space then --adjacent:隣り合っている(オリジナル関数)
    {config = List.map (\p -> if p.x==piece.x && p.y==piece.y then
                                {p|x=model.space.x, y=model.space.y}
                              else
                                p
                                  ) model.config --config:ピースの位置
    ,space = {x=piece.x, y=piece.y, n=0}} --space:空白の位置 n=0:space
  else
    model

unit = 50
pieceView: Piece -> Svg Msg
pieceView piece =
  g[onClick (Slide piece)
    ,transform ("translate("++(String.fromInt (unit*piece.x))
    ++ "," ++(String.fromInt (unit*piece.y))++")")] --translate(平行移動)(x座標, y座標)
    [
    rect[
         fill "skyblue"
         ,stroke "black"
         ,width (String.fromInt unit)
         ,height (String.fromInt unit)]
         []
  ,text_[x (String.fromInt (unit//2))
        ,y (String.fromInt (unit//2))][text(String.fromInt piece.n)]
  ]

confView: Config -> List (Svg Msg)
confView config =
  List.map pieceView config

view: Model -> Html Msg --viewで表示
view model =
  Html.div []
  [
  svg [width "400"
     ,height "400"]
     (confView model.config)
  ]
