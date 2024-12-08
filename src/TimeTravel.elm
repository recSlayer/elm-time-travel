module TimeTravel exposing (addTimeTravel)

import List
import Playground exposing (..)
import Set


type alias GenericGame model =
    { initialState : model, updateState : GenericUpdate model, view : GenericView model }


type alias GenericUpdate model =
    Computer -> model -> model


type alias GenericView model =
    Computer -> model -> List Shape


type alias TimeTravelModel model =
    { rawModel : model
    , paused : Bool
    , history : List Computer
    , historyPlaybackPosition : Int
    }


controlBarHeight =
    64


addTimeTravel : GenericGame model -> GenericGame (TimeTravelModel model)
addTimeTravel rawGame =
    { initialState = initialStateWithTimeTravel rawGame
    , updateState = updateWithTimeTravel rawGame
    , view = viewWithTimeTravel rawGame
    }


initialStateWithTimeTravel : GenericGame model -> TimeTravelModel model
initialStateWithTimeTravel rawGame =
    { rawModel = rawGame.initialState, paused = False, history = [], historyPlaybackPosition = 0 }


viewWithTimeTravel : GenericGame model -> GenericView (TimeTravelModel model)
viewWithTimeTravel rawGame computer model =
    let
        helpMessage =
            if model.paused then
                "Press R to resume, or click and drag to time travel"

            else
                "Press T to time travel"

        -- Creates a rectangle at the top of the screen, stretching from the
        -- left edge up to a specific position within the history timeline
        historyBar color opacity index =
            let
                width =
                    historyIndexToX computer index
            in
            rectangle color width controlBarHeight
                |> move (computer.screen.left + width / 2)
                    (computer.screen.top - controlBarHeight / 2)
                |> fade opacity
    in
    rawGame.view computer model.rawModel
        ++ [ historyBar black 0.3 maxVisibleHistory
           , historyBar lightGrey 0.6 (List.length model.history)
           , historyBar yellow 0.6 model.historyPlaybackPosition
           , words white helpMessage
                |> move 0 (computer.screen.top - controlBarHeight / 4)
           , words white "Press C to restart"
                |> move 0 (computer.screen.top - controlBarHeight * 3 / 4)
           ]


updateWithTimeTravel : GenericGame model -> GenericUpdate (TimeTravelModel model)
updateWithTimeTravel rawGame computer model =
    let
        model1 =
            if keyPressed "t" computer then
                { model | paused = True }

            else if keyPressed "r" computer then
                { model | paused = False, history = List.take model.historyPlaybackPosition model.history }

            else if keyPressed "c" computer then
                { model | paused = False, history = [], rawModel = rawGame.initialState }

            else
                model
    in
    if model1.paused then
        if computer.mouse.down then
            { model1 | historyPlaybackPosition = min (mousePosToHistoryIndex computer) (List.length model1.history), rawModel = replayHistory rawGame.updateState rawGame.initialState (List.take model1.historyPlaybackPosition model1.history) }

        else
            model1

    else
        { model1 | rawModel = rawGame.updateState computer model1.rawModel, history = model1.history ++ [ computer ], historyPlaybackPosition = List.length model1.history + 1 }


replayHistory : GenericUpdate model -> model -> List Computer -> model
replayHistory update initialState pastInputs =
    List.foldl update initialState pastInputs


keyPressed keyName computer =
    [ String.toLower keyName
    , String.toUpper keyName
    ]
        |> List.any (\key -> Set.member key computer.keyboard.keys)


maxVisibleHistory =
    2000



-- Converts an index in the history list to an x coordinate on the screen


historyIndexToX computer index =
    toFloat index / maxVisibleHistory * computer.screen.width



-- Converts the mouse's current position to an index within the history list


mousePosToHistoryIndex computer =
    (computer.mouse.x - computer.screen.left)
        / computer.screen.width
        * maxVisibleHistory
        |> round
