module Main where

import Array
import Signal exposing (Signal)
import Html exposing (Html)

import MapEditor
import View

main =
    let initialMap = Array.repeat MapEditor.mapHeight <| Array.repeat MapEditor.mapWidth 0
        initial = { selected = 0, map = initialMap }
    in start { initial = initial, update = MapEditor.update, view = View.view, inputs = [Signal.map (MapEditor.MapMouseDown) mapMouseInput] }

port mapMouseInput : Signal (Int, Int)

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
