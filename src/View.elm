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
    in Html.div [classPaletteView] <| List.indexedMap toHtml palette

selectedColorView : Int -> Html
selectedColorView i = Html.div [classSelectedView]
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
    in Html.div [classAppLayout] [mapView map address, controlView]

mapView : Array (Array Int) -> Signal.Address MapEditor.AppInput -> Html
mapView map address =
    let (ox, oy) = ((-w + MapEditor.tileSize) // 2, (h - MapEditor.tileSize) // 2)
        w = MapEditor.mapWidth * MapEditor.tileSize
        h = MapEditor.mapHeight * MapEditor.tileSize
        toForm i j c =
            Collage.move (toFloat (ox + i * MapEditor.tileSize), toFloat (oy + -j * MapEditor.tileSize))
            << flip Collage.filled (Collage.square (toFloat MapEditor.tileSize))
            <| getPaletteColor c
        forms = List.concat
            << Array.toList
            << Array.indexedMap (\j col -> Array.toList << Array.indexedMap (\i row -> toForm i j row) <| col)
            <| map
        mv = Html.div [classMapView, Html.id "map-view"] << flip (::) [] << Html.fromElement <| Collage.collage w h forms
    in Html.div [] [mv, encodedView map]

encodedView : Array (Array Int) -> Html
encodedView map =
    let encodeRow = Array.foldl (\a b -> b ++ (if b == "" then "" else ",") ++ toString a) ""
        encoded = (\a -> "[" ++ a ++ "]") << Array.foldl (\a b -> b ++ (if b == "" then "" else ",\n") ++ "[" ++ a ++ "]") ""
            << Array.map encodeRow
            <| map
    in Html.textarea [] [Html.text encoded]

classAppLayout = Html.class "app-layout"
classMapView = Html.class "map-view"
classControlContainer = Html.class "control-container"
classControlGroup = Html.class "control-group"
classControls = Html.class "controls"
classPaletteView = Html.class "palette-view"
classPaletteElement = Html.class "palette-element"
classSelectedView = Html.class "selected-view"
