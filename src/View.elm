module View where

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

import MapEditor


paletteColors : Array Color
paletteColors = Array.fromList
    [ Color.white
    , Color.red
    , Color.orange
    , Color.yellow
    , Color.green
    , Color.blue
    , Color.purple
    , Color.brown
    ]

getPaletteColor : Int -> Color
getPaletteColor i = Maybe.withDefault Color.white << Array.get i <| paletteColors

paletteView : Signal.Address MapEditor.AppInput -> Html
paletteView address =
    let toHtml i form =
            Html.div
                [classPaletteElement, Html.Events.onClick address (MapEditor.SelectColor i)]
                [Html.fromElement <| Collage.collage MapEditor.tileSize MapEditor.tileSize [form]]
    in Html.div [classPalette] <| List.indexedMap toHtml palette

selectedColorView : Int -> Html
selectedColorView i = Html.div [classSelected]
    << flip (::) []
    << Html.fromElement
    << Collage.collage MapEditor.tileSize MapEditor.tileSize
    << flip (::) []
    << flip Collage.filled (Collage.square (toFloat MapEditor.tileSize))
    <| getPaletteColor i

palette : List Collage.Form
palette = Array.toList
    << Array.map (flip Collage.filled (Collage.square (toFloat MapEditor.tileSize)))
    <| paletteColors

view : Signal.Address MapEditor.AppInput -> MapEditor.AppState -> Html
view address {selected, map} =
    let group label a =
            Html.div
                [classControlGroup]
                [Html.label [] [Html.text label], a]
        controlView =
            Html.div
                [classControlContainer]
                [group "tiles" <| paletteView address, group "selected" <| selectedColorView selected]
    in Html.div [classApp] [mapView map address, controlView, outputView map]

mapView : Array (Array Int) -> Signal.Address MapEditor.AppInput -> Html
mapView map address =
    let (ox, oy) = ((-w + MapEditor.tileSize) // 2, (h - MapEditor.tileSize) // 2)
        mapWidth = Array.length << Maybe.withDefault Array.empty << Array.get 0 <| map
        mapHeight = Array.length map
        w = mapWidth * MapEditor.tileSize
        h = mapHeight * MapEditor.tileSize
        toForm i j c =
            Collage.move (toFloat (ox + i * MapEditor.tileSize), toFloat (oy + -j * MapEditor.tileSize))
            << flip Collage.filled (Collage.square (toFloat MapEditor.tileSize))
            <| getPaletteColor c
        forms = List.concat
            << Array.toList
            << Array.indexedMap (\j col -> Array.toList << Array.indexedMap (\i row -> toForm i j row) <| col)
            <| map
    in Html.div [classMap, Html.id "map-view"] << flip (::) [] << Html.fromElement <| Collage.collage w h forms

outputView : Array (Array Int) -> Html
outputView map =
    let encodeRow = Array.foldl (\a b -> b ++ (if b == "" then "" else ",") ++ toString a) ""
        encoded = (\a -> "[" ++ a ++ "]") << Array.foldl (\a b -> b ++ (if b == "" then "" else ",\n") ++ "[" ++ a ++ "]") ""
            << Array.map encodeRow
            <| map
    in Html.textarea [classOutput, Html.readonly True] [Html.text encoded]

classApp = Html.class "app"
classMap = Html.class "map"
classOutput = Html.class "output"
classControlContainer = Html.class "control-container"
classControlGroup = Html.class "control-group"
classControls = Html.class "controls"
classPalette = Html.class "palette"
classPaletteElement = Html.class "palette-element"
classSelected = Html.class "selected"
