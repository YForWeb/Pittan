module First exposing(..)

import Browser
import Html exposing (..)
--(Html)にしてた時は Html.div [] [Html.button [][text "+"],~ Html.div,Html.text..にする必要がある
import Html.Events exposing (..)

main = Browser.sandbox {init=0, update=update, view=view} --init:初期化

type Msg = Plus | Minus
type alias Model = Int --
--Update 更新処理
update: Msg -> Model -> Model --メッセージとモデルを受け取ったら新しいモデルに書き換えられる
update msg model = --model：パズルそのもの
  case msg of
      Plus -> model+1
      Minus -> model-1

view: Model -> Html Msg --viewはcmodelを受けたらhtmlのページを出す役割だということを示す
view model = --viewで表示される --画面を構成
  div [] [button [onClick Plus][text "+"]
                  ,button[onClick Minus][text "-"]
                  ,text (String.fromInt model)] --何らかの入力がある
  --Html.h1 div [] [Html.text model] だとエラーが出る
  -- text: String, model: int
  --→更新される→viewで表示、、を繰り返す
