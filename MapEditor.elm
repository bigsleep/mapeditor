module MapEditor where

-- core
import Color exposing (Color)
import Graphics.Element as Element exposing (Element, show)
import Graphics.Collage as Collage
import List
import Signal exposing (Signal)

-- elm-html
import Html exposing (Html)

main = paletteView

tileSize : Int
tileSize = 60

paletteColors : List Color
paletteColors =
    [ Color.red
    , Color.orange
    , Color.yellow
    , Color.green
    , Color.blue
    , Color.purple
    , Color.brown
    ]

--port paletteView : Signal Html
paletteView =
    Html.div [] <| List.map (Html.fromElement << Collage.collage tileSize tileSize << flip (::) []) palette

palette : List Collage.Form
palette =
    List.map (flip Collage.filled (Collage.square (toFloat tileSize))) paletteColors
