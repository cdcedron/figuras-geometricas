module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo
type FloatingPic = Vector -> Vector -> Vector -> Picture
type Output a = a -> FloatingPic

-- el vector nulo
zero :: Vector
zero = (0,0)

half :: Vector -> Vector
half = (0.5 V.*)

-- comprender esta función es un buen ejericio.
hlines :: Vector -> Float -> Float -> [Picture]
hlines v@(x,y) mag sep = map (hline . (*sep)) [0..]
  where hline h = line [(x,y+h),(x+mag,y+h)] 

-- Una grilla de n líneas, comenzando en v con una separación de sep y
-- una longitud de l (usamos composición para no aplicar este
-- argumento)
grid :: Int -> Vector -> Float -> Float -> Picture
grid n v sep l = pictures [ls,translate 0 (l*toEnum n) (rotate 90 ls)]
  where ls = pictures $ take (n+1) $ hlines v sep l
  
-- figuras adaptables comunes
trian1 :: FloatingPic
trian1 a b c = line $ map (a V.+) [zero, half b V.+ c , b , zero]

trian2 :: FloatingPic
trian2 a b c = line $ map (a V.+) [zero, c, b,zero]

trianD :: FloatingPic
trianD a b c = line $ map (a V.+) [c, half b , b V.+ c , c]

rectan :: FloatingPic
rectan a b c = line [a, a V.+ b, a V.+ b V.+ c, a V.+ c,a]

simple :: Picture -> FloatingPic
simple p _ _ _ = p

fShape :: FloatingPic
fShape a b c = line . map (a V.+) $ [ zero,uX, p13, p33, p33 V.+ uY , p13 V.+ uY 
                 , uX V.+ 4 V.* uY ,uX V.+ 5 V.* uY, x4 V.+ y5
                 , x4 V.+ 6 V.* uY, 6 V.* uY, zero]    
  where p33 = 3 V.* (uX V.+ uY)
        p13 = uX V.+ 3 V.* uY
        x4 = 4 V.* uX
        y5 = 5 V.* uY
        uX = (1/6) V.* b
        uY = (1/6) V.* c

-- figura vacia
vacia :: FloatingPic
vacia a b c = blank

-- Dada una función que produce una figura a partir de un a y un vector
-- producimos una figura flotante aplicando las transformaciones
-- necesarias. Útil si queremos usar figuras que vienen de archivos bmp.
transf :: (a -> Vector -> Picture) -> a -> Vector -> FloatingPic
transf f d (xs,ys) a b c  = translate (fst a') (snd a') .
                             scale (magV b/xs) (magV c/ys) .
                             rotate ang $ f d (xs,ys)
  where ang = radToDeg $ argV b
        a' = a V.+ half (b V.+ c)

-- Semantica de cada operacion
rotar' :: FloatingPic -> FloatingPic
rotar' g a b c = g (a V.+ b) c (V.negate b)

espejar' :: FloatingPic -> FloatingPic
espejar' g a b c = g (a V.+ b) (V.negate b) c

rot45' :: FloatingPic -> FloatingPic
rot45' g a b c = g (a V.+ half (b V.+ c)) (half(b V.+ c)) (half(c V.- b))

encimar' :: FloatingPic -> FloatingPic -> FloatingPic 
encimar' g h a b c   = pictures [g a b c, h a b c]

apilar' :: Int -> Int -> FloatingPic -> FloatingPic -> FloatingPic 
apilar'  m n g h a b c = pictures [g (a V.+ ((n'/(m'+n')) V.* c)) b ((m'/(m'+n')) V.* c), h a b ((n'/(m'+n')) V.* c)]
                        where n' = toEnum n :: Float
                              m' = toEnum m :: Float

juntar' :: Int -> Int -> FloatingPic -> FloatingPic -> FloatingPic 
juntar' m n g h a b c = pictures [g a ((m'/(m'+n') V.* b)) c, h (a V.+ ((m'/(m'+n')) V.* b)) ((n'/(m'+n')) V.* b) c]
                        where n' = toEnum n :: Float
                              m' = toEnum m :: Float

--Interpretacion Geometrica
interp :: Output a -> Output (Dibujo a)
interp f (Basica d) = f d
interp f (Rotar d) = rotar' (interp f d)
interp f (Espejar d) = espejar' (interp f d) 
interp f (Rot45 d) = rot45' (interp f d) 
interp f (Encimar d e) = encimar' (interp f d) (interp f e)
interp f (Apilar m n d e) = apilar' m n (interp f d) (interp f e)
interp f (Juntar m n d e) = juntar' m n (interp f d) (interp f e)