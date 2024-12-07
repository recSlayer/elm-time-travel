-- Use arrow keys to move the turtle around.
--
-- Forward with UP and turn with LEFT and RIGHT.
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--


module Turtle exposing (game)

import List exposing (map)
import Playground exposing (..)


game =
    { initialState = initialState
    , updateState = update
    , view = view
    }


initialState =
    { x = 0
    , y = 0
    , angle = 0
    , trail = []
    , plotting = True
    }


view computer turtle =
    [ rectangle blue computer.screen.width computer.screen.height
    , image 96 96 "https://elm-lang.org/images/turtle.gif"
        |> move turtle.x turtle.y
        |> rotate turtle.angle
    ]
        ++ map (\p -> circle green 2 |> move p.x p.y) turtle.trail


update computer turtle =
    let
        x =
            turtle.x + 2 * (toY computer.keyboard * cos (degrees turtle.angle))

        y =
            turtle.y + 2 * (toY computer.keyboard * sin (degrees turtle.angle))

        angle =
            turtle.angle - 2 * toX computer.keyboard

        plotting =
            if computer.keyboard.space then
                not turtle.plotting

            else
                turtle.plotting
    in
    { turtle
        | x = x
        , y = y
        , angle = angle
        , trail =
            if turtle.plotting then
                { x = x, y = y } :: turtle.trail

            else
                turtle.trail
        , plotting = plotting
    }
