module Main where

import Cube exposing (..)
import Window
import Time exposing (fps, delay)
import Graphics.Element exposing (..)
import Keyboard

main : Signal Element
main =
  Signal.map view (Signal.foldp update state input)

input : Signal (Float, Keys)
input =
    let
        delta = Signal.map (\t -> t/60) (fps 50)
    in
      Signal.sampleOn delta (Signal.map2 (,) delta tap)


tap : Signal Keys
tap = 
  Signal.merge Keyboard.arrows
    (delay 50 (Signal.map ( \b -> { x = 0, y = 0 } ) Keyboard.arrows))
