module Main exposing (..)

import Asteroids
import Mario
import Playground
import Turtle



-- Converts the record-based { view, initialState, updateState } games this project uses into
-- an application that Elm knows how to run.
--


gameApplication game =
    Playground.game game.view game.updateState game.initialState



-- The main entry point for the app


main =
    Asteroids.game
        |> gameApplication
