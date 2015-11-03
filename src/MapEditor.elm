module MapEditor where

-- core
import Array exposing (Array)
import List
import Signal exposing (Signal)

mapWidth : Int
mapWidth = 16

mapHeight : Int
mapHeight = 16

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

type alias AppState =
    { selected : Int
    , map : Array (Array Int)
    }

type AppInput =
    SelectColor Int |
    MapMouseDown (Int, Int) |
    MapMouseUp (Int, Int)
