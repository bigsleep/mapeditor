module View where

-- core
import Array exposing (Array)
import Color exposing (Color)
import Graphics.Element as Element exposing (Element, show)
import Graphics.Collage as Collage
import Json.Decode
import List
import Signal exposing (Signal)
import String

-- elm-html
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events

import MapEditor

tileSize : Int
tileSize = 20

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

paletteView : Signal.Address Int -> Html
paletteView address =
    let toHtml i form =
            Html.div
                [classPaletteElement, Html.Events.onClick address i]
                [Html.fromElement <| Collage.collage tileSize tileSize [form]]
    in Html.div [classPalette] <| List.indexedMap toHtml palette

selectedColorView : Signal Int -> Signal Html
selectedColorView = 
    Signal.map
        <| Html.div [classSelected]
        << flip (::) []
        << Html.fromElement
        << Collage.collage tileSize tileSize
        << flip (::) []
        << flip Collage.filled (Collage.square (toFloat tileSize))
        << getPaletteColor

palette : List Collage.Form
palette = Array.toList
    << Array.map (flip Collage.filled (Collage.square (toFloat tileSize)))
    <| paletteColors

inputView : (Int -> MapEditor.AppInput) -> Signal.Address MapEditor.AppInput -> Html
inputView toAction address =
    let enterEventDecoder = Json.Decode.object2 (,) Html.Events.keyCode Html.Events.targetValue `Json.Decode.andThen`
            \(k, a) ->
                case (k, String.toInt a) of
                    (13, Ok i) -> Json.Decode.succeed << toAction <| i
                    (_, Err e) -> Json.Decode.fail e

        clickEventDecoder = Html.Events.targetValue `Json.Decode.andThen`
            \a ->
                case String.toInt a of
                    Ok i -> Json.Decode.succeed << toAction <| i
                    Err e -> Json.Decode.fail e

        onEnter = Html.Events.on "keypress" enterEventDecoder (Signal.message address)
        onClick = Html.Events.on "click" clickEventDecoder (Signal.message address)
        widthInput = Html.input [Attr.type' "number", onEnter, onClick] []
    in Html.div [] [widthInput]

view : Signal.Mailbox Int -> Signal.Address MapEditor.AppInput -> Signal (MapEditor.AppState -> Html)
view selectedMb address =
    let combineApp mapViewF controlViewF outputViewF map =
            Html.div [classApp] [mapViewF map, controlViewF map, outputViewF map]
        mv = mapView address
        cv = controlView selectedMb address
        ov = outputView
    in Signal.map3 combineApp mv cv ov

controlView : Signal.Mailbox Int -> Signal.Address MapEditor.AppInput -> Signal (MapEditor.AppState -> Html)
controlView selectedMb address =
    let controlViewF selectedColorV {map} =
        let mapWidth = Array.length << Maybe.withDefault Array.empty << Array.get 0 <| map
            mapHeight = Array.length map

            group label a =
                Html.div
                    [classControlGroup]
                    [Html.label [] [Html.text label], a]
            changeWidth w = MapEditor.ResizeMap (w, mapHeight)

            changeHeight h = MapEditor.ResizeMap (mapWidth, h)

        in Html.div
            [classControlContainer]
            [ group "tiles" <| paletteView selectedMb.address
            , group "selected" <| selectedColorV
            , group "width" <| inputView changeWidth address
            , group "height" <| inputView changeHeight address
            ]
    in Signal.map controlViewF (selectedColorView selectedMb.signal)


mapView : Signal.Address MapEditor.AppInput -> Signal (MapEditor.AppState -> Html)
mapView address =
    let mapViewF {map} =
        let (ox, oy) = ((-w + tileSize) // 2, (h - tileSize) // 2)
            mapWidth = Array.length << Maybe.withDefault Array.empty << Array.get 0 <| map
            mapHeight = Array.length map
            w = mapWidth * tileSize
            h = mapHeight * tileSize
            toForm i j c =
                Collage.move (toFloat (ox + i * tileSize), toFloat (oy + -j * tileSize))
                << flip Collage.filled (Collage.square (toFloat tileSize))
                <| getPaletteColor c
            forms = List.concat
                << Array.toList
                << Array.indexedMap (\j col -> Array.toList << Array.indexedMap (\i row -> toForm i j row) <| col)
                <| map
        in Html.div [classMap, Attr.id "map-view"] << flip (::) [] << Html.fromElement <| Collage.collage w h forms
    in Signal.constant mapViewF

outputView : Signal (MapEditor.AppState -> Html)
outputView =
    let outputViewF {map} =
        let encodeRow = Array.foldl (\a b -> b ++ (if b == "" then "" else ",") ++ toString a) ""
            encoded = (\a -> "[" ++ a ++ "]") << Array.foldl (\a b -> b ++ (if b == "" then "" else ",\n") ++ "[" ++ a ++ "]") ""
                << Array.map encodeRow
                <| map
        in Html.textarea [classOutput, Attr.readonly True] [Html.text encoded]
    in Signal.constant outputViewF

classApp = Attr.class "app"
classMap = Attr.class "map"
classOutput = Attr.class "output"
classControlContainer = Attr.class "control-container"
classControlGroup = Attr.class "control-group"
classControls = Attr.class "controls"
classPalette = Attr.class "palette"
classPaletteElement = Attr.class "palette-element"
classSelected = Attr.class "selected"
