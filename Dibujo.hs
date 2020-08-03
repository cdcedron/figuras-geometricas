module Dibujo where

data Dibujo a = Basica a | Rotar (Dibujo a) | Espejar (Dibujo a) | Rot45 (Dibujo a)
               | Encimar (Dibujo a) (Dibujo a)
               | Apilar Int Int (Dibujo a) (Dibujo a) 
               | Juntar Int Int (Dibujo a) (Dibujo a)
               deriving Show

--Esquemas para la manipulación de Figuras.

-- ver un a como una Figura
pureDibe :: a -> Dibujo a
pureDibe a = Basica a

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica a) = Basica $ f a
mapDib f (Rotar d) = Rotar $ mapDib f d
mapDib f (Espejar d) = Espejar $ mapDib f d
mapDib f (Rot45 d) = Rot45 $ mapDib f d
mapDib f (Encimar d e) = Encimar (mapDib f d) (mapDib f e)
mapDib f (Apilar n m d e) = Apilar n m (mapDib f d) (mapDib f e)
mapDib f (Juntar n m d e) = Juntar n m (mapDib f d) (mapDib f e)

cambia :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
cambia f (Basica a) = f a
cambia f (Rotar d) = Rotar $ cambia f d
cambia f (Rot45 d) = Rot45 $ cambia f d
cambia f (Encimar d e) = Encimar (cambia f d)  (cambia f e)
cambia f (Apilar n m d e) = Apilar n m (cambia f d) (cambia f e)
cambia f (Juntar n m d e) = Juntar n m (cambia f d) (cambia f e)

-- estructura general para la semántica
sem :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Int -> Int -> b -> b -> b) -> 
       (Int -> Int -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b
sem bas rot esp r45 api jun enc (Basica d) = bas d 
sem bas rot esp r45 api jun enc (Rotar d) = rot (sem bas rot esp r45 api jun enc d)
sem bas rot esp r45 api jun enc (Espejar d) = esp (sem bas rot esp r45 api jun enc d) 
sem bas rot esp r45 api jun enc (Rot45 d) = r45 (sem bas rot esp r45 api jun enc d)
sem bas rot esp r45 api jun enc (Encimar d e) = enc (sem bas rot esp r45 api jun enc d) (sem bas rot esp r45 api jun enc e)
sem bas rot esp r45 api jun enc (Apilar n m d e) = api n m (sem bas rot esp r45 api jun enc d) (sem bas rot esp r45 api jun enc e)
sem bas rot esp r45 api jun enc (Juntar n m d e) = jun n m (sem bas rot esp r45 api jun enc d) (sem bas rot esp r45 api jun enc e)

-- Funciones

type Pred a = a -> Bool

-- dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
limpia :: Pred a -> a -> Dibujo a -> Dibujo a
limpia p bas = mapDib(\x -> if p x then bas else x)

-- alguna básica satisface el predicado
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = sem p id id id (\_ _ d e -> d || e) (\_ _ d e -> d || e) (\ d e -> d || e)

-- todas las básicas satisfacen el predicado
allDib :: Pred a -> Dibujo a -> Bool
allDib p = sem p id id id (\_ _ d e -> d && e) (\_ _ d e -> d && e) (\ d e -> d && e)

-- describe la figura 
desc :: (a -> String) -> Dibujo a -> String
desc s = sem s (\a -> "rot ("++ a ++")") (\a -> "esp ("++ a ++")") (\a -> "r45 ("++ a ++")") (\_ _ a1 a2 -> "api ("++ a1 ++")" ++ "("++ a2 ++")") (\_ _ a1 a2 -> "jun ("++ a1 ++")" ++ "("++ a2 ++")") (\a1 a2 -> "enc ("++ a1 ++")" ++ "("++ a2 ++")")

-- junta todas las figuras básicas de un dibujo
every :: Dibujo a -> [a]
every = sem (\a -> [a]) id id id (\_ _ a1 a2 -> a1 ++ a2) (\_ _ a1 a2 -> a1 ++ a2) (\a1 a2 -> a1 ++ a2)

-- cuenta la cantidad de veces que aparecen las básicas en una figura
contar :: Eq a => Dibujo a -> [(a,Int)]
contar = sem (\a -> [(a,1)]) id id id (\_ _ d e -> contar' d e)
                                      (\_ _ d e -> contar' d e)
                                      (\d e -> contar' d e) 
                                                                                
-- función auxiliar de contar
contar' :: Eq a => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
contar' [(a1,n1)] [(a2,n2)]  =  if a1 == a2 then [(a1,(n1 + n2))] else (a1,n1) : [(a2,n2)]
contar' [(a1,n1)] ((a2,n2):ys)  =  if a1 == a2 then contar' [(a1,(n1 + n2))] ys else (a2,n2):contar' [(a1,n1)] ys
contar' ((a1,n1):xs) ((a2,n2):ys) = contar' xs (contar' [(a1,n1)] ((a2,n2):ys))

-- Definición de predicados sobre figuras.

-- hay 4 rotaciones seguidas (empezando en el tope)
esRot360 :: Pred (Dibujo a)
esRot360 (Basica a) = False
esRot360 (Rotar(Rotar(Rotar(Rotar d)))) = True  
esRot360 (Rotar d) = (esRot360 d)
esRot360 (Espejar d) = (esRot360 d)
esRot360 (Rot45 d) = (esRot360 d)
esRot360 (Encimar d e) = (esRot360 d) || (esRot360 e)
esRot360 (Apilar n m d e) = (esRot360 d) || (esRot360 e)
esRot360 (Juntar n m d e) = (esRot360 d) || (esRot360 e)

-- hay 2 espejados seguidos (empezando en el tope)
esFlip2 :: Pred (Dibujo a)
esFlip2 (Basica a) = False  
esFlip2 (Espejar(Espejar d)) = True
esFlip2 (Rotar d) = (esFlip2 d)
esFlip2 (Espejar d) = (esFlip2 d)
esFlip2 (Rot45 d) = (esFlip2 d)
esFlip2 (Encimar d e) = (esFlip2 d) || (esFlip2 e)
esFlip2 (Apilar n m d e) = (esFlip2 d) || (esFlip2 e)
esFlip2 (Juntar n m d e) = (esFlip2 d) || (esFlip2 e)

-- Definición de función que aplica un predicado y devuelve un error indicando fallo o una figura si no hay el error.
-- la cadena que se toma como parámetro es la descripción del error.
check :: Pred (Dibujo a) -> String -> Dibujo a -> Either String (Dibujo a)
check a s d = if a d then Left s else Right d

-- aplica todos los chequeos y acumula todos los errores, sólo devuelve la figura si no hubo ningún error.
todoBien :: Eq a => Dibujo a -> Either [String] (Dibujo a)
todoBien d = if (esRot360 d) || (esFlip2 d) then Left (todoBien' d) else Right d 

-- función auxiliar de todoBien
todoBien' :: Dibujo a -> [String]
todoBien' (Basica a) = []
todoBien' (Rotar(Rotar(Rotar(Rotar d)))) = "Rot360":(todoBien' d)
todoBien' (Espejar(Espejar d)) = "Flip2":(todoBien' d)
todoBien' (Rotar d) = todoBien' d
todoBien' (Espejar d) = todoBien' d
todoBien' (Rot45 d) = todoBien' d
todoBien' (Encimar d e) = todoBien' d ++ todoBien' e
todoBien' (Apilar n m d e) = todoBien' d ++ todoBien' e
todoBien' (Juntar n m d e) = todoBien' d ++ todoBien' e

--Definir funciones que corrigen los errores detectados:

noRot360 :: Dibujo a -> Dibujo a
noRot360 (Basica a) = (Basica a)
noRot360 (Rotar(Rotar(Rotar(Rotar d)))) = noRot360 d
noRot360 (Rotar d) = Rotar (noRot360 d)
noRot360 (Espejar d) = Espejar (noRot360 d)
noRot360 (Rot45 d) = Rot45 (noRot360 d)
noRot360 (Encimar d e) = Encimar (noRot360 d) (noRot360 e)
noRot360 (Apilar n m d e) = Apilar n m (noRot360 d) (noRot360 e)
noRot360 (Juntar n m d e) = Juntar n m (noRot360 d) (noRot360 e)

noFlip2  :: Dibujo a -> Dibujo a
noFlip2 (Basica a) = (Basica a)
noFlip2 (Espejar(Espejar d)) = (noFlip2 d)
noFlip2 (Rotar d) = Rotar (noFlip2 d)
noFlip2 (Espejar d) = Espejar (noFlip2 d)
noFlip2 (Rot45 d) = Rot45 (noFlip2 d)
noFlip2 (Encimar d e) = Encimar (noFlip2 d) (noFlip2 e)
noFlip2 (Apilar n m d e) = Apilar n m (noFlip2 d) (noFlip2 e)
noFlip2 (Juntar n m d e) = Juntar n m (noFlip2 d) (noFlip2 e)