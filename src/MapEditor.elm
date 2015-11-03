module MapEditor where

-- core
import Array exposing (Array)
import Color exposing (Color)
import Debug
import Json.Encode
import Json.Decode exposing ((:=))
import Graphics.Element as Element exposing (Element, show)
import Graphics.Collage as Collage
import List
import Signal exposing (Signal)

-- elm-html
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events

main =
    let initialMap = Array.repeat mapHeight <| Array.repeat mapWidth 0
        initial = { selected = 0, map = initialMap }
    in start { initial = initial, update = update, view = view, inputs = [Signal.map (MapMouseDown) mapMouseInput] }

mapWidth = 16
mapHeight = 16

tileSize : Int
tileSize = 20

paletteColors : List Color
paletteColors =
    [ Color.white
    , Color.red
    , Color.orange
    , Color.yellow
    , Color.green
    , Color.blue
    , Color.purple
    , Color.brown
    ]

paletteView : Signal.Address AppInput -> Html
paletteView address =
    let toHtml i form = Html.div [classPaletteElement, Html.Events.onClick address (SelectColor i)] [Html.fromElement <| Collage.collage tileSize tileSize [form]]
    in Html.div [classPaletteView] <| List.indexedMap toHtml palette

selectedColorView : Int -> Html
selectedColorView i =
    Html.div [classSelectedView] << flip (::) [] << Html.fromElement << Collage.collage tileSize tileSize << flip (::) [] << flip Collage.filled (Collage.square (toFloat tileSize)) <| getFromList paletteColors i Color.white

palette : List Collage.Form
palette =
    List.map (flip Collage.filled (Collage.square (toFloat tileSize))) paletteColors

getFromList : List a -> Int -> a -> a
getFromList xs i default =
    case (xs, i) of
        ([], _) -> default
        (a :: _, 0) -> a
        (_ :: xs', i) -> if i < 0 then default else getFromList xs' (i - 1) default



view : Signal.Address AppInput -> AppState -> Html
view address {selected, map} =
    let group label a =
            Html.div [classControlGroup] [Html.label [] [Html.text label], a]
        controlView = Html.div [classControlContainer] [group "tiles" <| paletteView address, group "selected" <| selectedColorView selected]
    in Html.div [classAppLayout] [mapView map address, controlView]

mapView : Array (Array Int) -> Signal.Address AppInput -> Html
mapView map address =
    let (ox, oy) = ((-w + tileSize) // 2, (h - tileSize) // 2)
        w = mapWidth * tileSize
        h = mapHeight * tileSize
        toForm i j c =
            Collage.move (toFloat (ox + i * tileSize), toFloat (oy + -j * tileSize))
            << flip Collage.filled (Collage.square (toFloat tileSize))
            <| getFromList paletteColors c Color.white
        forms = List.concat
            << Array.toList
            << Array.indexedMap (\j col -> Array.toList << Array.indexedMap (\i row -> toForm i j row) <| col)
            <| map
    in Html.div [classMapView, Html.id "map-view"] << flip (::) [] << Html.fromElement <| Collage.collage w h forms

update : AppInput -> AppState -> AppState
update input {selected, map} = 
    case input of
        SelectColor c -> { selected = c, map = map }
        MapMouseDown (x, y) ->
            let x' = x // tileSize
                y' = y // tileSize
                _ = Debug.log "" (x', y')
                map' = 
                    case Array.get y' map of
                        Just col -> Array.set y' (Array.set x' selected col) map
                        Nothing -> map
                out = { selected = selected, map = map' }
            in out
        MapMouseUp (x, y) -> { selected = selected, map = map }

type alias AppState =
    { selected : Int
    , map : Array (Array Int)
    }

type AppInput =
    SelectColor Int |
    MapMouseDown (Int, Int) |
    MapMouseUp (Int, Int)

port mapMouseInput : Signal (Int, Int)

classAppLayout = Html.class "app-layout"
classMapView = Html.class "map-view"
classControlContainer = Html.class "control-container"
classControlGroup = Html.class "control-group"
classControls = Html.class "controls"
classPaletteView = Html.class "palette-view"
classPaletteElement = Html.class "palette-element"
classSelectedView = Html.class "selected-view"


type alias App action model =
    { initial : model
    , view : Signal.Address action -> model -> Html
    , update : action -> model -> model
    , inputs : List (Signal action)
    }

start : App action model -> Signal Html
start config =
    let actions = Signal.mailbox Nothing
    
        address = Signal.forwardTo actions.address Just

        sig = Signal.mergeMany (actions.signal :: List.map (Signal.map Just) config.inputs)

        update' a m =
            case a of
                Just action -> config.update action m
                Nothing -> m

        model = Signal.foldp update' config.initial sig 

    in Signal.map (config.view address) model
