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

type alias Mat a = Array (Array a)

update : AppInput -> AppState -> AppState
update input {map} = 
    case input of
        PutTile c (x, y) ->
            case Array.get y map of
                Just col -> {map = Array.set y (Array.set x c col) map}
                Nothing -> {map = map}

        Move (x, y) (dx, dy) ->
            if (x, y) /= (dx, dy) 
                then getFromMat (x, y) map
                        |> Maybe.map (\c -> {map = updateMat (dx, dy) c map})
                        |> Maybe.map (\m -> {map = updateMat (x, y) 0 m.map})
                        |> Maybe.withDefault {map = map}
                else {map = map}

        ResizeMap (w, h) ->
            let w' = min (max w minMapWidth) maxMapWidth
                h' = min (max h minMapHeight) maxMapHeight
            in {map = resizeMat w' h' 0 map}

type alias AppState =
    { map : Array (Array Int)
    }

type AppInput
    = PutTile Int (Int, Int)
    | Move (Int, Int) (Int, Int)
    | ResizeMap (Int, Int)

initializeMat : Int -> Int -> a -> Mat a
initializeMat w h x = Array.repeat h <| Array.repeat w x

resizeMat : Int -> Int -> a -> Mat a -> Mat a
resizeMat w h x m =
    let resize size x a =
            let size0 = Array.length a
            in if | size > size0 -> Array.append a (Array.repeat (size - size0) x)
                  | size < size0 -> Array.slice 0 size a
                  | otherwise -> a
        emptyRow = Array.repeat w x
    in resize h emptyRow << Array.map (resize w x) <| m

updateMat : (Int, Int) -> a -> Mat a -> Mat a
updateMat (x, y) a m =
    case Array.get y m of
        Just col -> Array.set y (Array.set x a col) m
        otherwise -> m

getFromMat : (Int, Int) -> Mat a -> Maybe a
getFromMat (x, y) m =
    case Array.get y m of
        Just col -> Array.get x col
        Nothing -> Nothing
