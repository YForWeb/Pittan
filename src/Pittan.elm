module Pittan exposing (..)

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
                   , startedAt: {x: Float, y: Float}
                   , nowAt: {x: Float, y: Float}
                   , moving: Maybe Int
                   }


type alias Conf = List Piece

type alias Piece = { id: Int
                   , x: Int
                   , y: Int
                   , c: String
                   , used: Bool
                   }

type Msg = PDown Int {x: Float, y:Float}
    | PUp {x:Float, y:Float}
    | PMove {x:Float, y:Float}

unit = 60

init : () -> (Model, Cmd Msg)
init _ =
    ( Model initConf {x=0, y=0} {x=0, y=0} Nothing
    , Cmd.none
    )

initConf = [Piece 0 0 2 "あ" False
          ,Piece 1 0 3 "い" False]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PDown id pos ->
            ( {model | startedAt = pos, moving = Just id}
            , Cmd.none)
        PMove pos ->
            let
                dummy = Debug.log "move" pos
            in
              ( {model | nowAt = pos}
              , Cmd.none)
        PUp pos ->
            let
                x = ((floor  (pos.x / (toFloat unit))) )
                y = ((floor  (pos.y / (toFloat unit))))
                newConf = List.map (\p -> if p.id == (Maybe.withDefault (-1) <| model.moving) then
                                            {p | x=x, y=y}
                                         else
                                            p
                                    ) model.conf
            in

            ( {model | startedAt = {x=0, y=0}
              , nowAt = {x=0, y=0}
              , moving = Nothing
              , conf = newConf
              }
            , Cmd.none)


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

latticeView : Int -> Int -> List (Svg Msg)
latticeView sizeX sizeY =
    let
        hLineView : Int -> Svg Msg
        hLineView y =
            line [ x1 (String.fromInt (unit*2))
                 , y1 (String.fromInt (unit*(y+2)))
                 , x2 (String.fromInt (unit*(sizeX+2)))
                 , y2 (String.fromInt (unit*(y+2)))
                 , stroke "black"
                 ][]
        vLineView x =
            line [ x1 (String.fromInt (unit*(x+2)))
                 , y1 (String.fromInt (unit*2))
                 , x2 (String.fromInt (unit*(x+2)))
                 , y2 (String.fromInt (unit*(sizeY+2)))
                 , stroke "black"
                 ][]
    in
        (List.map hLineView (List.range 0 sizeY))++
            (List.map vLineView (List.range 0 sizeX))


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
              ((List.map (\p -> pieceView p model) model.conf)++
                   (latticeView 5 5))
        ]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
