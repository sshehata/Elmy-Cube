module Cube (Keys, state, view, update) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)


-- MODEL

type alias Model =
  ( Cube
  , Pipe
  )

type alias Keys =
  { x : Int
  , y : Int
  }

type alias Cube =
  { width : Float
  , height : Float
  , color : Color
  , y : Float
  , vy : Float
  , x : Float
  }

type alias Pipe =
  { x : Float
  , h : Float
  , w : Float
  , color : Color
  , gap : Float
  }

type alias Window =
  { w : Float
  , h : Float
  }

type alias Ground =
  { w : Float
  , h : Float
  }

type Position =
  TOP | BOTTOM


cube : Cube
cube =
  { width = 30.0
  , height = 30.0
  , color = rgb 37 186 0
  , y = 0.0
  , vy = -6.0
  , x = -window.w/2 + 30.0
  }

pipe : Pipe
pipe =
  { x = window.w/2
  , h = 300.0
  , w = 50.0
  , color = (rgb 26 175 93)
  , gap = 150.0
  }

state : Model
state =
  ( cube
  , pipe
  )

window : Window
window =
  { w = 600.0
  , h = 600.0
  }
  
ground : Ground
ground =
  { w = window.w
  , h = window.h/8
  }

topPipeHeight : Float
topPipeHeight =
  window.h - pipe.h - pipe.gap



-- UPDATE

update : (Float, Keys) -> Model -> Model
update (dt, keys) (cube, pipe) =
  if checkCollision (cube, pipe)
  then (cube, pipe)
  else ( cube
          |> physics dt
          |> gravity keys
  
       , pipe
          |> movingPipes dt

       )


movingPipes : Float -> Pipe -> Pipe
movingPipes dt pipe =
  let
      x' = pipe.x - dt * 20
  in
      { pipe | x =
        if x' < -(window.w/2 + pipe.w)
        then window.w/2
        else x'
      }


gravity : Keys -> Cube -> Cube
gravity keys cube =
  { cube | vy =
      if keys.y > 0
      then min 40 (cube.vy + 20)
      else max -20 (cube.vy - 4)
  }


physics : Float -> Cube -> Cube
physics dt cube =
  { cube | y =
      cube.y + dt * cube.vy
  }


checkCollision : Model -> Bool
checkCollision state =
  collidesPipes state || collidesGround state


-- TODO: Rewrite this
collidesPipes : Model -> Bool
collidesPipes (cube, pipe) =
  cube.x + cube.width >= pipe.x   &&
  cube.x <= pipe.x + pipe.w       && 
  (cube.y <= -window.h/2 + pipe.h ||
   cube.y + cube.height >= window.h/2 - topPipeHeight)


collidesGround : Model -> Bool
collidesGround (cube, pipe) =
  cube.y <= -window.h/2 + ground.h
  
    

-- VIEW

view : Model -> Element
view (cube, pipe) =
  let 
      (w, h) = (round window.w, round window.h)
  in

       collage w h
         [ rect window.w window.h
             |> filled (rgb 173 212 244)

         , rect pipe.w topPipeHeight
             |> filled pipe.color
             |> move ( pipe.x
                     , pipeOffset pipe TOP
                     )

         , rect pipe.w pipe.h
             |> filled pipe.color
             |> move ( pipe.x
                     , pipeOffset pipe BOTTOM
                     )

         , rect ground.w ground.h
             |> filled (rgb 206 177 113)
             |> move ( 0, -window.h/2 + ground.h/2)

         , rect cube.width cube.height
             |> filled cube.color
             |> move ( cube.x
                     , cube.y
                     )

         ]


pipeOffset : Pipe -> Position -> Float
pipeOffset pipe position =
  case position of
    TOP ->
      window.h/2 - topPipeHeight/2

    BOTTOM ->
      -window.h/2 + pipe.h/2
