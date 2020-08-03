module Basico.Escher where
import Basico.Extra
import Dibujo
import Interp

data Bas = Trian1 | Trian2 | TrianD | Rectan | FShape | Vacia 
           --Basicas Extras
         | Extra1 | Extra2 | Extra3 | Extra4 | Extra5  | Extra6 | Extra7 | Extra8 | Extra9 | Curvita 


figura :: Dibujo Bas
figura = escher 4 Trian2

figExtras :: Dibujo Bas
figExtras = cuarteto (escher 4 Extra1) (escher 4 Extra5) (escher 4 Extra7) (cuarteto (escher 4 Extra2) (escher 4 Extra3) (escher 4 Extra4) (escher 4 Extra6))

interpBas :: Output Bas
interpBas Trian1 = trian1 
interpBas Trian2 = trian2
interpBas TrianD = trianD
interpBas Rectan = rectan
interpBas FShape = fShape
interpBas Vacia = vacia
--Extras
interpBas Extra1 = extra1
interpBas Extra2 = extra2
interpBas Extra3 = extra3
interpBas Extra4 = extra4
interpBas Extra5 = extra5
interpBas Extra6 = extra6
interpBas Extra7 = extra7
interpBas Extra8 = extra8
interpBas Extra9 = extra9
interpBas Curvita = curvita

--Combinadores

-- composición n-veces de una función con sí misma
comp :: (a -> a) -> Int -> a -> a
comp f n | n == 0 = id 
         | otherwise = comp f(n-1) . f

-- rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 a = comp Rotar 2 a

r270 :: Dibujo a -> Dibujo a
r270 a = comp Rotar 3 a

-- pone una Figura sobre la otra, ambas ocupan el mismo espacio
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) a b = Apilar 1 1 a b

-- pone una Figura al lado de la otra, ambas ocupan el mismo espacio
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) a b = Juntar 1 1 a b

-- superpone una Figura con otra
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) a b = Encimar a b

-- dada una Figura la repite en cuatro cuadrantes
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto a b c d = (.-.) ((///) a b ) ((///) c d)

-- una Figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a 
encimar4 a = (^^^) ((^^^) (Rotar a) (a)) ((^^^) (r180 a) (r270 a)) 

-- cuadrado con la misma Figura rotada $i$ por $90$ para $i \in \{1..3\}$.
ciclar :: Dibujo a -> Dibujo a
ciclar a = cuarteto a (Rotar a) (r180 a) (r270 a)

 -- Reconstruccion del gráfico de Escher

type Escher = Bas

-- el dibujo u
dibujo_u :: Dibujo Escher -> Dibujo Escher
dibujo_u p = Encimar (Encimar p1 (Rotar p1)) (Encimar (r180 p1) (r270 p1))
         where p1 = Espejar (Rot45 p)
               p2 = Rotar (Rotar(Rotar p1))
-- el dibujo t
dibujo_t :: Dibujo Escher -> Dibujo Escher
dibujo_t p = Encimar p (Encimar p1 p2)
         where p1 = Espejar (Rot45 p)
               p2 = Rotar(Rotar(Rotar p1))

-- lado con nivel de detalle
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 1 p = cuarteto (Basica Vacia) (Basica Vacia) (Rotar (dibujo_t p)) (dibujo_t p)
lado 2 p = cuarteto (lado 1 p) (lado 1 p) (Rotar (dibujo_t p)) (dibujo_t p)
lado n p = cuarteto (lado (n-1) p) (lado (n-1) p) (Rotar (dibujo_t p)) (dibujo_t p)

-- esquina con nivel de detalle en base a la figura p
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 1 p = cuarteto (Basica Vacia) (Basica Vacia) (Basica Vacia) (dibujo_u p) 
esquina 2 p = cuarteto (esquina 1 p) (lado 1 p) (Rotar(lado 1 p)) (dibujo_u p) 
esquina n p = cuarteto (esquina (n-1) p) (lado (n-1) p) (Rotar(lado (n-1) p)) (dibujo_u p)

noneto p q r s t u v w x = Apilar 1 2 
                           (Juntar 1 2 p (Juntar 1 1 q r)) 
                           (Apilar 1 1 (Juntar 1 2 s (Juntar 1 1 t u)) (Juntar 1 2 v (Juntar 1 1 w x)))

-- el dibujo de Escher:
escher :: Int -> Escher -> Dibujo Escher
escher n p = noneto (esquina n (Basica p)) (lado n (Basica p)) (Espejar(esquina n (Basica p))) (Rotar(lado n (Basica p)))
                    (dibujo_u (Basica p)) (r270(lado n (Basica p))) (r180(Espejar(esquina n (Basica p)))) (r180(lado n (Basica p)))
                    (r180(esquina n (Basica p)))
