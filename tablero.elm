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
ficha = { x = 4 , y = 2}

-- UPDATE
goto (x,y) f = 
    let (x',y') = direccion (dondeEsta (x,y)) f
    in { f | x <- min 9 (max 1 (f.x + x')) , 
                     y <- min 9 (max 1 (f.y + y'))}

dondeEsta (x, y) =
  let (x', y') = (toFloat x, toFloat y)
  in (min 9 (max 1 (1 + floor ((x' - 45)/50))),
      min 9 (max 1 (9 - floor ((y' - 65)/50))))

direccion (x,y) f =
  let (x',y') = (toFloat x, toFloat y)
      dist = abs (x' - f.x) + abs (y' - f.y)
  in if | dist == 1 -> (x' - f.x, y' - f.y)
        | otherwise -> (0,0)

-- DISPLAY
altablero f = 
           move (50 * f.x - 280, 50 * f.y - 240) (filled red (circle 10))
sombra (x,y) =
   let (x', y') = (toFloat x, toFloat y)
   in  move (50 * x' - 280, 50 * y' - 240) (filled lightGrey (circle 10))

render ficha raton =
  let fila = group (List.map 
                   (\n -> (move (50*n-280,0) (outlined (solid brown) (square 40)))) [1..9])
  in flow right [collage 600 600 
       ((List.map 
         (\n -> (move (0,50*n - 240) fila)) [1..9]) ++ [sombra raton] ++ [altablero ficha]),
           asText raton]

-- Ahora sí
main : Signal Element
main = Signal.map2 render (Signal.foldp goto ficha (Signal.sampleOn Mouse.clicks Mouse.position)) (Signal.map dondeEsta Mouse.position)





