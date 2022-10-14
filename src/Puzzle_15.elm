module Puzzle_15 exposing (..)
--module ファイル名
import Html exposing (..)
--↑必要ないが、エラーを防ぐため
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing(..)
import Browser

main = Browser.sandbox {init=init, update=update, view=view} --決まり事として

type alias Model = List{x:Int, y:Int, n:Int}
type alias Position = {x:Int, y:Int} --クリックされた場所
type Msg = Slide Position --Toggle:入れ替える

init: Model
init = List.map(\i->{x=modBy 5 i, y=i//5, n=i})
      (List.range 0 24)

update: Msg -> Model -> Model --update:メッセージを受けて、現在のモデルの状態を見て、新しいモデルを作る
update msg model =  --=model だとmsgを受け取っても何もしない
  case msg of
    Slide pos -> slide model pos --{model | onoff=not model.onoff}
        --not model --not: trueとfalseを切り替える演算子

slide: Model -> Position -> Model
slide model pos =
  --匿名関数
  let
    others = List.filter(\p->not (p.x==pos.x && p.y==pos.y)) model
    clicked = List.filter(\p-> (p.x==pos.x && p.y==pos.y)) model
  in
  --p:modelに入っている一つ一つほピースのデータ
  --pos:Position クリックされた場所のx座標、y座標
  List.map (\p -> if (p.x - pos.x)^2 + (p.y - pos.y)^2 ==1 && p.n==0 then --クリックされた場所とその周辺で距離が１で、スペースのところ
                    --クリックされた場所とピースの場所が1以内ならon offが変わる
                      {p|x=pos.x, y=pos.y} --on offを切り替える
                    else
                      p
                      ) model --

unit = 100

piece p =
           rect [x (String.fromInt(p.x*unit)) --"300" --cx,cy, は文字列を受け取らなくてはならない
                ,y (String.fromInt(p.y*unit)) --"300"
                ,width (String.fromInt unit)--"100" 数字はできるだけ描かない方がいい（変数を作る）
                ,height (String.fromInt unit)
                ,stroke (if p.n==0 then  --縁取り
                          "white"
                        else
                        "black"
                    )
                ,rx (String.fromInt (unit//10))
                ,ry (String.fromInt (unit//10))
                ,fill (if p.n==0 then
                       "white"
                      else
                       "pink"
                      )
                ,onClick (Slide {x=p.x, y=p.y})] --onClickは引数1つなはずなので()をつける
                [] --最後に空パックを入れる

view: Model -> Html Msg
view model =
  svg [width "600"
      ,height "600"]
      (List.map piece model) --modelに入っている一つ一つの要素に対してcircleというリスト

--型は大文字、変数は小文字始まり
