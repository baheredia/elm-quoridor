import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import Signal
import Text (asText)
import Mouse
import Keyboard

-- MODEL
ficha : {x : Float, y : Float}
ficha = { x = 5 , y = 2}

-- UPDATE
goto {x,y} f = 
    let (x',y') = (toFloat x, toFloat y)
    in { f | x <- min 9 (max 1 (f.x + x')) , 
                     y <- min 9 (max 1 (f.y + y'))}

-- DISPLAY
altablero f = 
           move (50 * f.x - 280, 50 * f.y - 240) (filled red (circle 10))
render ficha =
  let fila = group (List.map 
                   (\n -> (move (50*n-280,0) (outlined (solid brown) (square 40)))) [1..9])
  in collage 600 600 
       ((List.map 
         (\n -> (move (0,50*n - 240) fila)) [1..9]) ++ [altablero ficha])

-- Ahora sí
main : Signal Element
main = Signal.map render (Signal.foldp goto ficha Keyboard.arrows)





