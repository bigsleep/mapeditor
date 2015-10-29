module MapEditor where

-- core
import Array exposing (Array)
import Color exposing (Color)
import Graphics.Element as Element exposing (Element, show)
import Graphics.Collage as Collage
import List
import Signal exposing (Signal)

-- elm-html
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events

-- start-app
import StartApp.Simple as StartApp

main =
    let initialMap = Array.repeat mapHeight <| Array.repeat mapWidth 0
        initial = { selected = 0, map = initialMap }
    in StartApp.start { model = initial, view = view, update = update }

mapWidth = 16
mapHeight = 16

tileSize : Int
tileSize = 20

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
    let toHtml i form = Html.div [classPaletteElement, Html.Events.onClick address i] [Html.fromElement <| Collage.collage tileSize tileSize [form]]
    in Html.div [] <| List.indexedMap toHtml palette

selectedColorView : Int -> Html
selectedColorView i =
    Html.div [] << flip (::) [] << Html.fromElement << Collage.collage tileSize tileSize << flip (::) [] << flip Collage.filled (Collage.square (toFloat tileSize)) <| getFromList paletteColors i Color.red

palette : List Collage.Form
palette =
    List.map (flip Collage.filled (Collage.square (toFloat tileSize))) paletteColors

getFromList : List a -> Int -> a -> a
getFromList xs i default =
    case (xs, i) of
        ([], _) -> default
        (a :: _, 0) -> a
        (_ :: xs', i) -> if i < 0 then default else getFromList xs' (i - 1) default



view : Signal.Address Int -> AppState -> Html
view address {selected, map} =
    let group label a =
            Html.div [classControlGroup] [Html.label [] [Html.text label], a]
        controlView = Html.div [classControlContainer] [group "tiles" <| paletteView address, group "selected" <| selectedColorView selected]
    in Html.div [classAppLayout] [mapView map, controlView]

mapView : Array (Array Int) -> Html
mapView map =
    let (ox, oy) = ((-w + tileSize) // 2, (h + tileSize) // 2)
        w = mapWidth * tileSize
        h = mapHeight * tileSize
        toForm i j c =
            Collage.move (toFloat (ox + i * tileSize), toFloat (oy + -j * tileSize))
            << flip Collage.filled (Collage.square (toFloat tileSize))
            <| getFromList paletteColors c Color.red
        forms = List.concat
            << Array.toList
            << Array.indexedMap (\j col -> Array.toList << Array.indexedMap (\i row -> toForm i j row) <| col)
            <| map
    in Html.div [classMapView] << flip (::) [] << Html.fromElement <| Collage.collage w h forms

update : Int -> AppState -> AppState
update input {selected, map} = { selected = input, map = map }

type alias AppState =
    { selected : Int
    , map : Array (Array Int)
    }

classAppLayout = Html.class "app-layout"
classMapView = Html.class "map-view"
classControlContainer = Html.class "control-container"
classControlGroup = Html.class "control-group"
classControls = Html.class "controls"
classPaletteElement = Html.class "palette-element"
