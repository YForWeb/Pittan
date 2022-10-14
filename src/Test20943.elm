module Test20943 exposing (..)
--module ファイル名
import Html exposing (..)
--↑必要ないが、エラーを防ぐため
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing(..)
import Browser
import Html.Events.Extra.Pointer as Pointer

main = Browser.sandbox {init=init, update=update, view=view} --決まり事として

type alias Model = List{x:Int, y:Int, onoff:Bool}--Bool --論理型true or false
type alias Position = {x:Int, y:Int} --クリックされた場所
type Msg = Toggle Position --Toggle:入れ替える
           |DragStart {xp:Float, yp:Float}
           |Dragging {xp:Float, yp:Float}
           |DragEnd {xp:Float, yp:Float}

init: Model
init = {-List.map(\i->{x=modBy 5 i, y=i//5, onoff=False})
      (List.range 0 24) -}

  [{x=1,y=2,onoff=False}
        ,{x=1,y=3,onoff=False}
        ,{x=1,y=4,onoff=False}
        ,{x=2,y=1,onoff=False}
        ,{x=2,y=2,onoff=False}
        ,{x=2,y=3,onoff=False}
        ,{x=2,y=4,onoff=False}
        ,{x=2,y=5,onoff=False}
        ,{x=3,y=2,onoff=False}
        ,{x=3,y=3,onoff=False}
        ,{x=3,y=4,onoff=False}
        ,{x=3,y=5,onoff=False}
        ,{x=3,y=6,onoff=False}
        ,{x=4,y=3,onoff=False}
        ,{x=4,y=4,onoff=False}
        ,{x=4,y=5,onoff=False}
        ,{x=4,y=6,onoff=False}
        ,{x=4,y=7,onoff=False}
        ,{x=5,y=2,onoff=False}
        ,{x=5,y=3,onoff=False}
        ,{x=5,y=4,onoff=False}
        ,{x=5,y=5,onoff=False}
        ,{x=5,y=6,onoff=False}
        ,{x=6,y=1,onoff=False}
        ,{x=6,y=2,onoff=False}
        ,{x=6,y=3,onoff=False}
        ,{x=6,y=4,onoff=False}
        ,{x=6,y=5,onoff=False}
        ,{x=7,y=2,onoff=False}
        ,{x=7,y=3,onoff=False}
        ,{x=7,y=4,onoff=False}
         ]--True


update: Msg -> Model -> Model --update:メッセージを受けて、現在のモデルの状態を見て、新しいモデルを作る
update msg model =  --=model だとmsgを受け取っても何もしない
  case msg of
    Toggle pos -> toggle model pos --{model | onoff=not model.onoff}
        --not model --not: trueとfalseを切り替える演算子
    DragStart pos ->
      let
        dumy = Debug.log "DragStart" pos
      in
        model

    Dragging pos ->
      let
        dumy = Debug.log "Dragging" pos
      in
        model

    DragEnd pos ->
      let
        dumy = Debug.log "DragEnd" pos
      in
        model

relativePos : Pointer.Event -> { xp:Float, yp:Float }
relativePos event =
    {xp = Tuple.first event.pointer.offsetPos
     ,yp = Tuple.second event.pointer.offsetPos}

toggle: Model -> Position -> Model
toggle model pos =
  --匿名関数
  --p:modelに入っている一つ一つほピースのデータ
  --pos:Position クリックされた場所のx座標、y座標
  List.map (\p -> if (abs(p.x-pos.x)==0 || abs(p.y-pos.y)==0)&& (abs(p.x-pos.x) <=1 && abs(p.y-pos.y) <=1) then --押された場所と同じ場所を持っているピース
                    --if(p.x - pos.x)^2 + (p.y - pos.y)^2 <=1
                    --クリックされた場所とピースの場所が1以内ならon offが変わる
                      {p|onoff=not p.onoff} --on offを切り替える
                    else
                      p
                      ) model --

unit = 100

piece p =
        {- circle [cx (String.fromInt(p.x*unit)) --"300" --cx,cy, は文字列を受け取らなくてはならない
                ,cy (String.fromInt(p.y*unit)) --"300"
                ,r "50"-}
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
                [] --最後に空パックを入れる

view: Model -> Html Msg
view model =
  svg [width "1200"
      ,height "1200"
      ,Pointer.onDown (relativePos >> DragStart)
      ,Pointer.onMove (relativePos >> Dragging)
      ,Pointer.onUp (relativePos >> DragEnd)]

      (List.map piece model) --modelに入っている一つ一つの要素に対してcircleというリスト

--型は大文字、変数は小文字始まり
