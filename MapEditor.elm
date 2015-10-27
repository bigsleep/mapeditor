module MapEditor where

-- core
import Color exposing (Color)
import Graphics.Element as Element exposing (Element, show)
import Graphics.Collage as Collage
import List
import Signal exposing (Signal)

-- elm-html
import Html exposing (Html)
import Html.Events

-- start-app
import StartApp.Simple as StartApp

main = StartApp.start { model = 0, view = view, update = curry fst }

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

paletteView : Signal.Address Int -> Html
paletteView address =
    let toHtml i form = Html.div [Html.Events.onClick address i] [Html.fromElement <| Collage.collage tileSize tileSize [form]]
    in Html.div [] <| List.indexedMap toHtml palette

selectedColorView : Int -> Html
selectedColorView i =
    Html.fromElement << Collage.collage tileSize tileSize << flip (::) [] << flip Collage.filled (Collage.square (toFloat tileSize)) <| getFromList paletteColors i Color.red

palette : List Collage.Form
palette =
    List.map (flip Collage.filled (Collage.square (toFloat tileSize))) paletteColors

port selectedColor : Signal Int


getFromList : List a -> Int -> a -> a
getFromList xs i default =
    case (xs, i) of
        ([], _) -> default
        (a :: _, 0) -> a
        (_ :: xs', i) -> if i < 0 then default else getFromList xs' (i - 1) default



view : Signal.Address Int -> Int -> Html
view address selection =
    Html.div [] [paletteView address, Html.hr [] [], selectedColorView selection]
