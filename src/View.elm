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

type InputMode
    = InputModePutTile
    | InputModeMove

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

tileColorView : Signal Int -> Signal Html
tileColorView = 
    Signal.map
        <| Html.div [classTile]
        << flip (::) []
        << Html.fromElement
        << Collage.collage tileSize tileSize
        << flip (::) []
        << flip Collage.filled (Collage.square (toFloat tileSize))
        << getPaletteColor

modeView : Signal.Mailbox InputMode -> Signal Html
modeView {address, signal} =
    let onChange index = Html.Events.on "change" Html.Events.targetChecked (\_ -> Signal.message address index)
        radio mode (index, id, label) =
            Html.div []
                [ Html.input [Attr.id id, Attr.type' "radio", Attr.checked (index == mode), onChange index] []
                , Html.label [Attr.for id] [Html.text label]
                ]
        modeViewF mode = Html.div [] <| List.map (radio mode)
            [ (InputModePutTile, "input_mode_put_tile", "put tile")
            , (InputModeMove, "input_mode_move", "move")
            ]
    in Signal.map modeViewF signal

palette : List Collage.Form
palette = Array.toList
    << Array.map (flip Collage.filled (Collage.square (toFloat tileSize)))
    <| paletteColors

inputView : Int -> (Int -> MapEditor.AppInput) -> Signal.Address MapEditor.AppInput -> Html
inputView value toAction address =
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
        widthInput = Html.input [Attr.type' "number", onEnter, onClick, Attr.value <| toString value] []
    in Html.div [] [widthInput]

view : Signal.Mailbox Int
    -> Signal.Mailbox InputMode
    -> Signal (Maybe ((Int, Int), (Int, Int)))
    -> Signal.Address MapEditor.AppInput
    -> Signal (MapEditor.AppState -> Html)
view tileMb modeMb dragSig address =
    let combineApp mapViewF controlViewF outputViewF map =
            Html.div [classApp] [mapViewF map, controlViewF map, outputViewF map]
    in Signal.map3 combineApp (mapView address dragSig) (controlView tileMb modeMb address) outputView

controlView : Signal.Mailbox Int -> Signal.Mailbox InputMode -> Signal.Address MapEditor.AppInput -> Signal (MapEditor.AppState -> Html)
controlView tileMb modeMb address =
    let controlViewF tileColorV modeV {map} =
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
            [ group "tiles" <| paletteView tileMb.address
            , group "tile" <| tileColorV
            , group "width" <| inputView mapWidth changeWidth address
            , group "height" <| inputView mapHeight changeHeight address
            , group "mode" <| modeV
            ]
    in Signal.map2 controlViewF (tileColorView tileMb.signal) (modeView modeMb)


mapView : Signal.Address MapEditor.AppInput
    -> Signal (Maybe ((Int, Int), (Int, Int)))
    -> Signal (MapEditor.AppState -> Html)
mapView address dragSig =
    let mapViewF drag {map} =
        let (ox, oy) = ((-w + tileSize) // 2, (h - tileSize) // 2)

            mapWidth = Array.length << Maybe.withDefault Array.empty << Array.get 0 <| map

            mapHeight = Array.length map

            w = mapWidth * tileSize

            h = mapHeight * tileSize

            map' =
                case drag of
                    Just ((x, y), _) -> MapEditor.updateMat (x, y) 0 map
                    Nothing -> map

            moveForm x y = Collage.move (toFloat (ox + x), toFloat (oy - y))

            toForm =
                flip Collage.filled (Collage.square (toFloat tileSize))
                << getPaletteColor

            toFormAt x y =
                moveForm x y << toForm

            toFormAt' i j =
                moveForm (i * tileSize) (j * tileSize) << toForm

            forms = List.concat
                << Array.toList
                << Array.indexedMap
                    (\j col -> Array.toList << Array.indexedMap (\i row -> toFormAt' i j row) <| col)
                <| map'

            addDragForm ((x, y), (dx, dy)) c = 
                let x' = x * tileSize + dx
                    y' = y * tileSize + dy
                    dragOutline = Collage.outlined (Collage.dashed Color.black) << Collage.square <| toFloat tileSize
                in List.append forms
                   [ toFormAt x' y' c
                   , moveForm x' y' <| dragOutline
                   ]

            forms' =
                case drag of
                    Just (a, d) -> MapEditor.getFromMat a map
                        |> Maybe.map (addDragForm (a, d))
                        |> Maybe.withDefault forms
                    Nothing -> forms

        in Html.div [classMap, Attr.id "map-view"] << flip (::) [] << Html.fromElement <| Collage.collage w h forms'
    in Signal.map mapViewF dragSig

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
classTile = Attr.class "tile"
