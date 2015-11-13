module Main where

import Array
import Debug
import Signal exposing (Signal, (~))
import Html exposing (Html)

import MapEditor
import View

main =
    let initialMap = Array.repeat MapEditor.initialMapHeight <| Array.repeat MapEditor.initialMapWidth 0
        initial = { map = initialMap }

        tileMb = Signal.mailbox 0

        modeMb = Signal.mailbox View.InputModePutTile

        putTileSig = toPutTileSignal modeMb.signal tileMb.signal mapMouseInput

        moveSig = toMoveSignal modeMb.signal mapMouseInput

        sig = Signal.merge putTileSig moveSig

    in start { initial = initial, update = MapEditor.update, view = View.view tileMb modeMb, inputs = [sig] }

type alias MouseEvent =
    { eventType : String
    , position : (Int, Int)
    }

port mapMouseInput : Signal MouseEvent

toPutTileSignal : Signal View.InputMode -> Signal Int -> Signal MouseEvent -> Signal MapEditor.AppInput
toPutTileSignal modeInput tileInput mouseInput =
    let toAppInput (i, me) =
            let (x, y) = me.position
                x' = x // View.tileSize
                y' = y // View.tileSize
            in MapEditor.PutTile i (x', y')

        decider = Signal.map ((==) View.InputModePutTile) modeInput

        mouseInput' =
            filterBy decider { eventType = "noevent", position = (0, 0) }
            <| Signal.filter (\me -> me.eventType == "mousedown") { eventType = "noevent", position = (0, 0) }
            <| mouseInput
        
    in Signal.map toAppInput <| Signal.sampleOn mouseInput' (Signal.map2 (,) tileInput mouseInput')

toMoveSignal : Signal View.InputMode -> Signal MouseEvent -> Signal MapEditor.AppInput
toMoveSignal modeInput mouseInput =
    let toAppInput dnd =
            case dnd of
                DnDDrop a d -> Debug.log "move" <| Just <| MapEditor.Move a (fst d // View.tileSize, snd d // View.tileSize)
                otherwise -> Nothing

        decider = Signal.map ((==) View.InputModeMove) modeInput

        mouseInput' =
            filterBy decider { eventType = "noevent", position = (0, 0) }
            <| mouseInput

        selectSig =
            let f {eventType, position} prev = 
                case eventType of
                    "mousedown" -> Just (fst position // View.tileSize, snd position // View.tileSize)
                    otherwise -> prev
            in Signal.foldp f Nothing mouseInput'

    in Signal.filterMap identity (MapEditor.PutTile 0 (0, 0)) <| Signal.map toAppInput <| toDnDEventSignal mouseInput' selectSig

type DnDEvent a
    = DnDNoEvent
    | DnDDrag a (Int, Int)
    | DnDDrop a (Int, Int)

toDnDEventSignal : Signal MouseEvent -> Signal (Maybe a) -> Signal (DnDEvent a)
toDnDEventSignal mouse target =
    let sig = Signal.sampleOn mouse (Signal.map2 (,) mouse target)

        difference (ax, ay) (bx, by) = (ax - bx, ay - by)

        toDnD ({eventType, position}, a) (s, prev) =
            let _ = Debug.log "ev" (s, eventType, a)
            in case (s, eventType, a) of
                (Just start, "mousemove", Just x) -> (Just start, DnDDrag x (difference position start))
                (Just start, "mouseup", Just x) -> (Nothing, DnDDrop x (difference position start))
                (Nothing, "mousedown", Just x) -> (Just position, DnDDrag x (0, 0))
                otherwise -> (Nothing, DnDNoEvent)
    in Signal.map snd << Signal.foldp toDnD (Nothing, DnDNoEvent) <| sig

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

filterBy : Signal Bool -> a -> Signal a -> Signal a
filterBy decider default sig
    = Signal.map snd
    <| Signal.filter fst (True, default)
    <| Signal.sampleOn sig
    <| Signal.map2 (,) decider sig
