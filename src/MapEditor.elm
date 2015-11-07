module MapEditor where

-- core
import Array exposing (Array)
import List
import Signal exposing (Signal)

initialMapWidth : Int
initialMapWidth = 16

initialMapHeight : Int
initialMapHeight = 16

minMapWidth : Int
minMapWidth = 4

minMapHeight : Int
minMapHeight = 4

maxMapWidth : Int
maxMapWidth = 40

maxMapHeight : Int
maxMapHeight = 40

tileSize : Int
tileSize = 20

update : AppInput -> AppState -> AppState
update input {selected, map} = 
    case input of
        SelectColor c -> { selected = c, map = map }
        MapMouseDown (x, y) ->
            let x' = x // tileSize
                y' = y // tileSize
                map' = 
                    case Array.get y' map of
                        Just col -> Array.set y' (Array.set x' selected col) map
                        Nothing -> map
                out = { selected = selected, map = map' }
            in out
        MapMouseUp (x, y) -> { selected = selected, map = map }

        ResizeMap (w, h) ->
            let w' = min (max w minMapWidth) maxMapWidth
                h' = min (max h minMapHeight) maxMapHeight
            in { selected = selected, map = resizeMap w' h' 0 map }

type alias AppState =
    { selected : Int
    , map : Array (Array Int)
    }

type AppInput =
    SelectColor Int |
    MapMouseDown (Int, Int) |
    MapMouseUp (Int, Int) |
    ResizeMap (Int, Int)

initializeMap : Int -> Int -> a -> Array (Array a)
initializeMap w h x = Array.repeat h <| Array.repeat w x

resizeMap : Int -> Int -> a -> Array (Array a) -> Array (Array a)
resizeMap w h x m =
    let resize size x a =
            let size0 = Array.length a
            in if | size > size0 -> Array.append a (Array.repeat (size - size0) x)
                  | size < size0 -> Array.slice 0 size a
                  | otherwise -> a
        emptyRow = Array.repeat w x
    in resize h emptyRow << Array.map (resize w x) <| m
