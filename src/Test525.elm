module Test525 exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Html exposing (Html)
import Browser

main = Browser.sandbox {init=init, update=update, view=view}

type alias Model = List{x:Int, y:Int, onoff:Bool}--Bool --論理型true or false
type alias Position = {x:Int, y:Int} --クリックされた場所
type Msg = Toggle Position --Toggle:入れ替える

init: Model
init = List.map(\i->{x=modBy 5 i, y=i//5, onoff=False})
      (List.range 0 24)

update: Msg -> Model -> Model --update:メッセージを受けて、現在のモデルの状態を見て、新しいモデルを作る
update msg model =  --=model だとmsgを受け取っても何もしない
  case msg of
    Toggle pos -> toggle model pos --{model | onoff=not model.onoff}
        --not model --not: trueとfalseを切り替える演算子

toggle: Model -> Position -> Model
toggle model pos =
  --匿名関数
  --p:modelに入っている一つ一つほピースのデータ
  --pos:Position クリックされた場所のx座標、y座標
  List.map (\p -> if (abs(p.x-pos.x)==0 && abs(p.y-pos.y)==0) then
                     {p|onoff=not p.onoff}
                  else
                      p
                      ) model --

unit = 100

piece p =
           rect [x (String.fromInt(p.x*unit)) --"300" --cx,cy, は文字列を受け取らなくてはならない
                ,y (String.fromInt(p.y*unit)) --"300"
                ,width (String.fromInt unit)--"100" 数字はできるだけ描かない方がいい（変数を作る）
                ,height (String.fromInt unit)
                ,stroke "black" --縁取り
                ,rx (String.fromInt (unit//10))
                ,ry (String.fromInt (unit//10))
                ,fill (if p.onoff then --modelがtrueならピンク
                        "pink"
                      else
                        "cyan"
                      )

                ,onClick (Toggle {x=p.x, y=p.y})] --onClickは引数1つなはずなので()をつける
                [] --最後に空パックを入れ
                ,text_[x (String.fromInt (unit//2))
                      ,y (String.fromInt (unit//2))][text(String.fromInt piece.n)]

view: Model -> Html Msg
view model =
  svg [width "600"
      ,height "600"]
      (List.map piece model)
