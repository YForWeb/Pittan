module Pittan2 exposing (..)

import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Html.Events.Extra.Pointer as P

main = Browser.element { init = init
                       , update = update
                       , view = view
                       , subscriptions = subscriptions
                       }

type alias Model = { conf: Conf
                   , candidates: List String
                   , board: Board
                   , startedAt: {x: Float, y: Float}
                   , nowAt: {x: Float, y: Float}
                   , moving: Maybe Int
                   , cursor: Int
                   }


type alias Conf = List Piece

type alias Piece = { id: Int
                   , x: Int
                   , y: Int
                   , c: String
                   , used: Bool
                   }

type alias Cell = { x: Int
                  , y: Int
                  }
type alias Board = List Cell

type Msg = PDown Int {x: Float, y:Float}
    | PUp {x:Float, y:Float}
    | PMove {x:Float, y:Float}
    | GenPiece Piece {x:Float, y:Float}
    | CursorPlus
    | CursorMinus

unit = 60

init : () -> (Model, Cmd Msg)
init _ =
    ( Model []
          aKaraN
          (shikaku++[Cell 7 4])
          {x=0, y=0} {x=0, y=0} Nothing 0
    , Cmd.none
    )

initConf : List Piece
initConf = [Piece 0 0 2 "あ" False
          ,Piece 1 0 3 "い" False]

aKaraN : List String
aKaraN = [ "あ", "い", "う", "え", "お"
         , "か", "き", "く", "け", "こ"
         , "さ", "し", "す", "せ", "そ"
         , "た", "ち", "つ", "て", "と"
         , "な", "に", "ぬ", "ね", "の"
         , "は", "ひ", "ふ", "へ", "ほ"
         , "ま", "み", "む", "め", "も"
         , "や", "ゆ", "よ"
         , "ら", "り", "る", "れ", "ろ"
         , "わ"
         , "ん"
         , "が", "ぎ", "ぐ", "げ", "ご"
         , "ざ", "じ", "ず", "ぜ", "ぞ"
         , "だ", "ぢ", "づ", "で", "ど"
         , "ば", "び", "ぶ", "べ", "ぼ"
         , "ぱ", "ぴ", "ぷ", "ぺ", "ぽ"
         , "ぁ", "ぃ", "ぅ", "ぇ", "ぉ"
         , "っ"
         , "ゃ", "ゅ", "ょ"
         ]

shikaku : Board
shikaku  =
    List.concat <|
        List.map (\x ->
                      List.map (\y -> Cell x y) <| List.range 3 6
                 )
            <| List.range 3 6


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PDown id pos ->
            ( { model | startedAt = pos
              , nowAt = pos
              , moving = Just id}
            , Cmd.none)
        PMove pos ->
              ( {model | nowAt = pos}
              , Cmd.none)
        PUp pos ->
            let
                x = ((floor  (pos.x / (toFloat unit))) )
                y = ((floor  (pos.y / (toFloat unit))))
                onCell = List.member (Cell x y) model.board
                newConf = if onCell then
                              List.map (\p -> if p.id == (Maybe.withDefault (-1) <| model.moving) then
                                                  {p | x=x, y=y}
                                              else
                                                  p
                                       ) model.conf
                          else
                              List.filter (\p -> p.id /= (Maybe.withDefault (-1) <| model.moving)) model.conf
            in
                ( {model | startedAt = {x=0, y=0}
                  , nowAt = {x=0, y=0}
                  , moving = Nothing
                  , conf = newConf
                  }
                , Cmd.none
                )
        GenPiece p pos ->
            ({ model | conf = p::model.conf
             , moving = Just (p.id)
             , startedAt = pos
             , nowAt = pos
             }
            , Cmd.none
            )
        CursorPlus ->
            ({ model | cursor = if model.cursor < 0 then
                                    model.cursor + 1
                                else
                                    model.cursor
             }
            , Cmd.none
            )
        CursorMinus ->
            ({ model | cursor= if model.cursor > -(List.length model.candidates) then
                                    model.cursor - 1
                                else
                                    model.cursor
             }
            , Cmd.none
            )



pieceView : Piece -> Model -> Svg Msg
pieceView piece model =
    let
        dx = case model.moving of
                  Nothing -> 0
                  Just id ->
                    if id == piece.id then
                      model.nowAt.x - model.startedAt.x
                    else
                      0
        dy = case model.moving of
                  Nothing -> 0
                  Just id ->
                    if id == piece.id then
                      model.nowAt.y - model.startedAt.y
                    else
                      0
        dstring = "translate(" ++
                  (String.fromFloat ((toFloat (piece.x*unit)) + dx)) ++
                  ", " ++
                  (String.fromFloat ((toFloat (piece.y*unit)) + dy)) ++ ")"
    in
        Svg.g [ transform dstring
              , P.onDown (\event -> PDown piece.id
                              { x=Tuple.first event.pointer.offsetPos
                              , y=Tuple.second event.pointer.offsetPos
                              }
                         )
              ]
            [ rect [ width (String.fromInt unit)
                   , height (String.fromInt unit)
                   , fill "gray"
                   , fillOpacity "0.3"
                   , stroke "black"
                   ]
                  []
            , text_ [ x (String.fromInt (unit//4))
                    , y (String.fromInt (2*unit//3))
                    , fontSize (String.fromInt (unit//2))
                    , stroke "black"
                    ]
                  [text piece.c]
            ]

cellView : Cell -> Svg Msg
cellView cell =
    rect [x (String.fromInt (unit*cell.x))
         ,y (String.fromInt (unit*cell.y))
         ,width (String.fromInt unit)
         ,height (String.fromInt unit)
         ,stroke "black"
         ,fill "none"
         ][]

boardView : Board -> Svg Msg
boardView board =
    g[] <| List.map cellView board

latticeView : Int -> Int -> Svg Msg
latticeView sizeX sizeY =
    let
        hLineView : Int -> Svg Msg
        hLineView y =
            line [ x1 "0"
                 , y1 (String.fromInt (unit*y))
                 , x2 (String.fromInt (unit*sizeX))
                 , y2 (String.fromInt (unit*y))
                 , stroke "black"
                 ][]
        vLineView x =
            line [ x1 (String.fromInt (unit*x))
                 , y1 "0"
                 , x2 (String.fromInt (unit*x))
                 , y2 (String.fromInt (unit*sizeY))
                 , stroke "black"
                 ][]
    in
        g[transform "translate(240, 240)"]
            <|(List.map hLineView (List.range 0 sizeY))++
                 (List.map vLineView (List.range 0 sizeX))


candView : Model -> Svg Msg
candView model =
    let
        letterView : Int -> String -> Svg Msg
        letterView i c =
            g [ transform ("translate (0" ++
                                           "," ++ (String.fromInt ((i+model.cursor)*unit)) ++ ")")
              , Svg.Attributes.clipPath "url(#candClip)"
              , P.onDown (\event -> (GenPiece (Piece (List.length model.conf) 1 (i+model.cursor+1) c True))
                              { x=Tuple.first event.pointer.offsetPos
                              , y=Tuple.second event.pointer.offsetPos
                              }
                         )
              ]
            [rect [ width (String.fromInt unit)
                  , height (String.fromInt unit)
                  , fill "gray"
                  , fillOpacity "0.3"
                  , stroke "black"
                  ,Svg.Attributes.clipPath "url(#candClip)"
                  ]
                 []
            , text_ [ x (String.fromInt (unit//4))
                    , y (String.fromInt (2*unit//3))
                    , fontSize (String.fromInt (unit//2))
                    , stroke "black"
                    , Svg.Attributes.clipPath "url(#candClip)"
                    ]
                 [text c]
            ]
    in
        g[transform ("translate(60," ++ (String.fromInt unit) ++ ")")]
            [g [ Svg.Attributes.clipPath "url(#candClip)"
               ]
                 (List.indexedMap letterView model.candidates)
            ,Svg.path [d ("M 0 -5 l " ++
                              (String.fromInt (unit)) ++ " 0 l " ++
                              (String.fromInt (-unit//2)) ++ " " ++
                              (String.fromInt (-unit//2)) ++ " z"
                         )
                      , fill "black"
                      , onClick CursorPlus
                      ]
                 []
            ,Svg.path [d ("M 0 "++ (String.fromInt (8*unit+5)) ++" l " ++
                              (String.fromInt (unit)) ++ " 0 l " ++
                              (String.fromInt (-unit//2)) ++ " " ++
                              (String.fromInt (unit//2)) ++ " z"
                         )
                      , fill "black"
                      , onClick CursorMinus
                      ]
                 []
            ]



view : Model -> Html Msg
view model =
    Html.div []
        [ svg [ width "1000"
              , height "1000"
              , P.onMove (\event -> PMove
                              { x=Tuple.first event.pointer.offsetPos
                              , y=Tuple.second event.pointer.offsetPos
                              }
                         )
              , P.onUp (\event -> PUp
                                { x=Tuple.first event.pointer.offsetPos
                                , y=Tuple.second event.pointer.offsetPos
                                }
                          )
              ]
              (
               [boardView model.board] ++
                   --[latticeView 5 5] ++
                   [Svg.clipPath [id "candClip"]
                        [ rect [x "0"
                               ,y "0"
                               ,width (String.fromInt unit)
                               ,height (String.fromInt (8*unit))
                               ][]
                        , Svg.path [d "M 0 0 l 100 0 l 0 480 l -100 0 Z"][]
                        ]
                   ] ++
                   [candView model] ++
                   (List.map (\p -> pieceView p model) model.conf)
              )
        ]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
