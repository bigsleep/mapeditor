module Main where

import Array
import Signal exposing (Signal, (~))
import Html exposing (Html)

import MapEditor
import View

main =
    let initialMap = Array.repeat MapEditor.initialMapHeight <| Array.repeat MapEditor.initialMapWidth 0
        initial = {  map = initialMap }

        selectedMB = Signal.mailbox 0

        signal = toAppInputSignal selectedMB.signal mapMouseInput

    in start { initial = initial, update = MapEditor.update, view = View.view selectedMB, inputs = [signal] }

type alias MouseEvent =
    { eventType : String
    , position : (Int, Int)
    }

port mapMouseInput : Signal MouseEvent

toAppInputSignal : Signal Int -> Signal MouseEvent -> Signal MapEditor.AppInput
toAppInputSignal selectionInput mouseInput =
    let toAppInput (i, me) =
            let (x, y) = me.position
                x' = x // View.tileSize
                y' = y // View.tileSize
            in MapEditor.PutTile i (x', y')
    in Signal.map toAppInput <| Signal.sampleOn mouseInput (Signal.map2 (,) selectionInput mouseInput)

type alias App action model =
    { initial : model
    , view : Signal.Address action -> Signal (model -> Html)
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

        modelSignal = Signal.foldp update' config.initial sig

    in config.view address ~ modelSignal
