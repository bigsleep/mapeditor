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

update : AppInput -> AppState -> AppState
update input {map} = 
    case input of
        PutTile c (x, y) ->
            case Array.get y map of
                Just col -> {map = Array.set y (Array.set x c col) map}
                Nothing -> {map = map}

        Move ps (dx, dy) -> {map = map}
{-
            let updates = List.filterMap sndJust << List.zip ps << getSomeFromMap ps map

                sndJust a =
                    case a of
                        (x, Just y) -> Just (x, y)
                        otherwise -> Nothing
            in {updateMap updates map}
-}

        ResizeMap (w, h) ->
            let w' = min (max w minMapWidth) maxMapWidth
                h' = min (max h minMapHeight) maxMapHeight
            in {map = resizeMap w' h' 0 map}

type alias AppState =
    { map : Array (Array Int)
    }

type AppInput
    = PutTile Int (Int, Int)
    | Move (List Int) (Int, Int)
    | ResizeMap (Int, Int)

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
