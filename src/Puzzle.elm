module Puzzle exposing (..)
--module ファイル名
import Html exposing (..)
--↑必要ないが、エラーを防ぐため
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing(..)
import Browser

main = Browser.sandbox {init=init, update=update, view=view} --決まり事として

type alias Model = {x:Int, y:Int, onoff:Bool}--Bool --論理型true or false
type Msg = Toggle --Toggle:入れ替える

init: Model
init = {x=2, y=3, onoff=False} --True

update: Msg -> Model -> Model --update:メッセージを受けて、現在のモデルの状態を見て、新しいモデルを作る
update msg model =  --=model だとmsgを受け取っても何もしない
  case msg of
    Toggle -> {model | onoff=not model.onoff}
        --not model --not: trueとfalseを切り替える演算子

unit = 100

view: Model -> Html Msg
view model =
  svg [width "600"
      ,height "600"]
      [
      circle [cx (String.fromInt(model.x*unit)) --"300" --cx,cy, は文字列を受け取らなくてはならない
              ,cy (String.fromInt(model.y*unit)) --"300"
              ,r "50"
              ,fill (if model.onoff then --modelがtrueならピンク
                      "pink"
                    else
                      "cyan"
                    )
              ,onClick Toggle]
              [] --最後に空パックを入れる
        ]

--型は大文字、変数は小文字始まり
