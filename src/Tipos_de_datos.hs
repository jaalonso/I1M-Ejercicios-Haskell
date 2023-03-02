-- Tipos_de_datos.hs
-- Ejercicios de tipos de datos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- En esta relación de ejercicio se estudian los tipos abstractos de
-- datos (TAD) tantos  los predefinidos (como booleanos, opcionales, pares y
-- listas) como definidos (árboles binarios). Se definen funciones sobre
-- los TAD y se verifican propiedades con QuickCheck (en el caso de los
-- TAD se definen sus generadores de elementos arbitrarios).

module Tipos_de_datos where

-- Se ocultas funciones que se van a definir.
import Prelude hiding ((++), or, reverse, filter)
import Test.QuickCheck
import Control.Applicative ((<|>), liftA2)

-- ---------------------------------------------------------------------
-- § Booleanos                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    implicacion :: Bool -> Bool -> Bool
-- tal que (implicacion b c) es la implicación entre b y c; su tabla es
--          | False | True
--    ------+-------+------
--    False | True  | True
--    True  | False | True
-- Por ejemplo,
--    implicacion False False  ==  True
--    implicacion True False   ==  False
-- ---------------------------------------------------------------------

implicacion :: Bool -> Bool -> Bool
implicacion False _ = True
implicacion True  b = b

-- ---------------------------------------------------------------------
-- Ejercicio 2. Redefinir la función
--    implicacion' :: Bool -> Bool -> Bool
-- usando la negación y la disyunción.
-- ---------------------------------------------------------------------

implicacion' :: Bool -> Bool -> Bool
implicacion' x y = y || not x

-- ---------------------------------------------------------------------
-- Ejercicio 3. Comprobar con QuickCheck que las funciones implicacion e
-- implicacion' son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_implicacion_implicacion' :: Bool -> Bool -> Property
prop_implicacion_implicacion' x y =
  implicacion x y === implicacion' x y

-- La comprobación es
--    λ> quickCheck prop_implicacion_implicacion'
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Maybe                                                            --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    orelse :: Maybe a -> Maybe a -> Maybe a
-- tal que (orelse m1 m2) es m1 si es no nulo y m2 en caso contrario.
-- Por ejemplo,
--    Nothing `orelse` Nothing == Nothing
--    Nothing `orelse` Just 5  == Just 5
--    Just 3  `orelse` Nothing == Just 3
--    Just 3  `orelse` Just 5  == Just 3
-- ---------------------------------------------------------------------

orelse :: Maybe a -> Maybe a -> Maybe a
orelse m@(Just _) _ = m
orelse _          n = n

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    mapMaybe :: (a -> b) -> Maybe a -> Maybe b
-- tal que (mapMaybe f m) es el resultado de aplicar f al contenido de
-- m. Por ejemplo,
--    mapMaybe (+ 2) (Just 6)  ==  Just 8
--    mapMaybe (+ 2) Nothing   ==  Nothing
-- ---------------------------------------------------------------------

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f (Just x) = Just (f x)
mapMaybe _ Nothing  = Nothing

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir, usndo (<|>), la función
--    orelse' :: Maybe a -> Maybe a -> Maybe a
-- tal que (orelse m1 m2) es m1 si es no nulo y m2 en caso
-- contrario. Por ejemplo,
--    Nothing `orelse'` Nothing == Nothing
--    Nothing `orelse'` Just 5  == Just 5
--    Just 3  `orelse'` Nothing == Just 3
--    Just 3  `orelse'` Just 5  == Just 3
-- ---------------------------------------------------------------------

orelse' :: Maybe a -> Maybe a -> Maybe a
orelse' = (<|>)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Comprobar con QuickCheck que las funciones implicacion e
-- implicacion' son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_orelse_orelse' :: Maybe Int -> Maybe Int -> Property
prop_orelse_orelse' x y =
  orelse x y === orelse' x y

-- La comprobación es
--    λ> quickCheck prop_orelse_orelse'
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir, usando <$>, la función
--    mapMaybe' :: (a -> b) -> Maybe a -> Maybe b
-- tal que (mapMaybe' f m) es el resultado de aplicar f al contenido de
-- m. Por ejemplo,
--    mapMaybe' (+ 2) (Just 6)  ==  Just 8
--    mapMaybe' (+ 2) Nothing   ==  Nothing
-- ---------------------------------------------------------------------

mapMaybe' :: (a -> b) -> Maybe a -> Maybe b
mapMaybe' = (<$>)

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    parMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
-- tal que (parMaybe m1 m2) es jost el par de los contenidos de m1 y m2
-- si ambos tienen contenido y Nothing en caso contrario. Por ejemplo,
--    parMaybe (Just 'x') (Just 'y')  ==  Just ('x','y')
--    parMaybe (Just 42) Nothing      ==  Nothing
-- ---------------------------------------------------------------------

parMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
parMaybe (Just x) (Just y) = Just (x, y)
parMaybe _        _        = Nothing

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    liftMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- tal que (liftMaybe f m1 m2) es el resultado de aplicar f a los
-- contenidos de m1 y m2 si tienen contenido y Nothing, en caso
-- contrario. Por ejemplo,
--    liftMaybe (*)  (Just 2)    (Just 3)      ==  Just 6
--    liftMaybe (*)  (Just 2)    Nothing       ==  Nothing
--    liftMaybe (++) (Just "ab") (Just "cd")   ==  Just "abcd"
--    liftMaybe elem (Just 'b')  (Just "abc")  ==  Just True
--    liftMaybe elem (Just 'p')  (Just "abc")  ==  Just False
-- ---------------------------------------------------------------------

liftMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe f (Just a) (Just b) = Just (f a b)
liftMaybe _ _        _        = Nothing

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir, usando liftMaybe, la función
--    parMaybe' :: Maybe a -> Maybe b -> Maybe (a, b)
-- tal que (parMaybe' m1 m2) es jost el par de los contenidos de m1 y m2
-- si ambos tienen contenido y Nothing en caso contrario. Por ejemplo,
--    parMaybe' (Just 'x') (Just 'y')  ==  Just ('x','y')
--    parMaybe' (Just 42) Nothing      ==  Nothing
-- ---------------------------------------------------------------------

parMaybe' :: Maybe a -> Maybe b -> Maybe (a, b)
parMaybe' = liftMaybe (,)

-- ---------------------------------------------------------------------
-- Ejercicio 12. Comprobar con QuickCheck que las funciones parMaybe e
-- parMaybe' son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_parMaybe_parMaybe' :: Maybe Int -> Maybe Int -> Property
prop_parMaybe_parMaybe' x y =
  parMaybe x y === parMaybe' x y

-- La comprobación es
--    λ> quickCheck prop_parMaybe_parMaybe'
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    sumaMaybes :: Maybe Int -> Maybe Int -> Maybe Int
-- tal que (sumaMaybes m1 m2) es la suma de los contenidos de m1 y m2 si
-- tienen contenido y Nothing, en caso contrario. Por ejemplo,
--    Just 2  `sumaMaybes` Just 3  == Just 5
--    Just 2  `sumaMaybes` Nothing == Nothing
--    Nothing `sumaMaybes` Just 3  == Nothing
--    Nothing `sumaMaybes` Nothing == Nothing
-- ---------------------------------------------------------------------

sumaMaybes :: Maybe Int -> Maybe Int -> Maybe Int
sumaMaybes = liftMaybe (+)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir (usando 'parMaybe', 'uncurry' y 'mapMaybe') la
-- función
--    sumaMaybes' :: Maybe Int -> Maybe Int -> Maybe Int
-- tal que (addMaybe's m1 m2) es la suma de los contenidos de m1 y m2 si
-- tienen contenido y Nothing, en caso contrario. Por ejemplo,
--    Just 2  `addMaybe's` Just 3  == Just 5
--    Just 2  `addMaybe's` Nothing == Nothing
--    Nothing `addMaybe's` Just 3  == Nothing
--    Nothing `addMaybe's` Nothing == Nothing
-- ---------------------------------------------------------------------

sumaMaybes' :: Maybe Int -> Maybe Int -> Maybe Int
sumaMaybes' x y = mapMaybe (uncurry (+)) (parMaybe x y)

-- ---------------------------------------------------------------------
-- Ejercicio 15. Comprobar con QuickCheck que las funciones sumaMaybes y
-- sumaMaybes' son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sumaMaybes_sumaMaybes' :: Maybe Int -> Maybe Int -> Property
prop_sumaMaybes_sumaMaybes' x y =
  sumaMaybes x y === sumaMaybes' x y

-- La comprobación es
--    λ> quickCheck prop_sumaMaybes_sumaMaybes'
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir, usando liftA2, la función
--    liftMaybe' :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- tal que (liftMaybe's f m1 m2) es el resultado de aplicar f a los
-- contenidos de m1 y m2 si tienen contenido y Nothing, en caso
-- contrario. Por ejemplo,
--    liftMaybe' (*)  (Just 2)    (Just 3)      ==  Just 6
--    liftMaybe' (*)  (Just 2)    Nothing       ==  Nothing
--    liftMaybe' (++) (Just "ab") (Just "cd")   ==  Just "abcd"
--    liftMaybe' elem (Just 'b')  (Just "abc")  ==  Just True
--    liftMaybe' elem (Just 'p')  (Just "abc")  ==  Just False
-- ---------------------------------------------------------------------

liftMaybe' :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe' = liftA2

-- ---------------------------------------------------------------------
-- § Pares                                                            --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    aplicaAmbas :: (a -> b) -> (a -> c) -> a -> (b, c)
-- tal que (aplicaAmbas f g x) es el par obtenido aplicándole a x las
-- funciones f y g. Por ejemplo,
--    aplicaAmbas (+ 1) (* 2) 7  ==  (8,14)
-- ---------------------------------------------------------------------

aplicaAmbas :: (a -> b) -> (a -> c) -> a -> (b, c)
aplicaAmbas f g a = (f a, g a)

-- ---------------------------------------------------------------------
-- § Listas                                                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    (++) :: [a] -> [a] -> [a]
-- tal que (xs ++ ys) es la concatenación de xs e ys. Por ejemplo,
--    [2,3] ++ [4,5,1]  ==  [2,3,4,5,1]
-- ---------------------------------------------------------------------

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    or :: [Bool] -> Bool
-- tal que (or xs) se verifca si algún elemento de xs es verdadero. Por
-- ejemplo,
--    or [False,True,False]   ==  True
--    or [False,False,False]  ==  False
-- ---------------------------------------------------------------------

or :: [Bool] -> Bool
or []           = False
or (True : _)   = True
or (False : bs) = or bs

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    reverse :: [a] -> [a]
-- tal que (reverse xs) es la inversa de xs. Por ejemplo,
--    reverse [4,2,5]  ==  [5,2,4]
-- ---------------------------------------------------------------------

reverse :: [a] -> [a]
reverse []       = []
reverse (x : xs) = reverse xs ++ [x]

-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir (sin usar reverse ni ++) la función
--    reverseAcc :: [a] -> [a] -> [a]
-- tal que (reverseAcc xs ys) es la concatención de xs y la inversa de
-- ys. Por ejemplo,
--    reverseAcc [3,2] [7,5,1]  ==  [1,5,7,3,2]
-- ---------------------------------------------------------------------

reverseAcc :: [a] -> [a] -> [a]
reverseAcc acc []       = acc
reverseAcc acc (x : xs) = reverseAcc (x : acc) xs

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir, usando reverseAcc, la función
--    reverse' :: [a] -> [a]
-- tal que (reverse' xs) es la inversa de xs. Por ejemplo,
--    reverse' [4,2,5]  ==  [5,2,4]
-- ---------------------------------------------------------------------

reverse' :: [a] -> [a]
reverse' = reverseAcc []

-- ---------------------------------------------------------------------
-- Ejercicio 23. Comprobar con QuickCheck que las funciones reverse y
-- reverse' son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_reverse_reverse' :: [Int] -> Property
prop_reverse_reverse' xs =
  reverse xs === reverse' xs

-- La comprobación es
--    λ> quickCheck prop_reverse_reverse'
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 24. Comparar la eficiencia de reverse y reverse' calculando
-- el tiempo de las siguientes evaluaciones
--    last (reverse [1..10^4])
--    last (reverse' [1..10^4])
-- ---------------------------------------------------------------------

-- La omparación es
--    λ> ;set +s
--    λ> last (reverse [1..10^4])
--    1
--    (6.25 secs, 8,759,415,640 bytes)
--    λ> last (reverse' [1..10^4])
--    1
--    (0.01 secs, 2,321,888 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--    filter :: (a -> Bool) -> [a] -> [a]
-- tal que (filter p xs) es la lista de los elementos de xs que cumplen
-- la propiedad p. Por ejmplo,
--    filter even [4,5,2]  ==  [4,2]
--    filter odd  [4,5,2]  ==  [5]
-- ---------------------------------------------------------------------

filter :: (a -> Bool) -> [a] -> [a]
filter _ []       = []
filter p (x : xs)
  | p x         = x : filter p xs
  | otherwise   = filter p xs

-- ---------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--    divisores :: Integral a => a -> [a]
-- tal que (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 24  ==  [1,2,3,4,6,8,12,24]
-- ---------------------------------------------------------------------

divisores :: Integral a => a -> [a]
divisores n = filter (\x -> mod n x == 0) [1 .. n]

-- ---------------------------------------------------------------------
-- Ejercicio 27. Definir la función
--    esPrimo :: Integral a => a -> Bool
-- tal que (esPrimo n) se verifica si n esprimo. Por ejemplo,
--    esPrimo 7  ==  True
--    esPrimo 9  ==  False
-- ---------------------------------------------------------------------

esPrimo :: Integral a => a -> Bool
esPrimo n = divisores n == [1, n]

-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la lista
--    milPrimos :: [Int]
-- formada por los 1000 primeros números primos.
-- ---------------------------------------------------------------------

milPrimos :: [Int]
milPrimos = take 1000 (filter esPrimo [1 ..])

-- ---------------------------------------------------------------------
-- § Árboles binarios                                                 --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 29. Definir el tipo de datos Arbol para los árboles
-- binarios, con valores sólo en las hojas.
-- ---------------------------------------------------------------------

data Arbol a = Hoja a
             | Nodo (Arbol a) (Arbol a)
  deriving (Eq, Show)

-- En los ejemplos se usarán los siguientes árboles
arbol1, arbol2, arbol3, arbol4 :: Arbol Int
arbol1 = Hoja 1
arbol2 = Nodo (Hoja 2) (Hoja 4)
arbol3 = Nodo arbol2 arbol1
arbol4 = Nodo arbol2 arbol3

-- ---------------------------------------------------------------------
-- Ejercicio 30. Definir la función
--    altura :: Arbol a -> Int
-- tal que (altura t) es la altura del árbol t. Por ejemplo,
--    λ> altura (Nodo (Hoja 3) (Nodo (Nodo (Hoja 1) (Hoja 7)) (Hoja 2)))
--    3
-- ---------------------------------------------------------------------

altura :: Arbol a -> Int
altura (Hoja _)   = 0
altura (Nodo l r) = 1 + max (altura l) (altura r)

-- ---------------------------------------------------------------------
-- Ejercicio 31. Definir la función
--    mapArbol :: (a -> b) -> Arbol a -> Arbol b
-- tal que (mapArbol f t) es el árbolo obtenido aplicando la función f a
-- los elementos del árbol t. Por ejemplo,
--    λ> mapArbol (+ 1) (Nodo (Hoja 2) (Hoja 4))
--    Nodo (Hoja 3) (Hoja 5)
-- ---------------------------------------------------------------------

mapArbol :: (a -> b) -> Arbol a -> Arbol b
mapArbol f (Hoja a)   = Hoja (f a)
mapArbol f (Nodo l r) = Nodo (mapArbol f l) (mapArbol f r)

-- ---------------------------------------------------------------------
-- Ejercicio 32. Definir la función
--    mismaForma :: Arbol a -> Arbol b -> Bool
-- tal que (mismaForma t1 t2) se verifica si t1 y t2 tienen la misma
-- estructura. Por ejemplo,
--    mismaForma arbol3 (mapArbol (* 10) arbol3)  ==  True
--    mismaForma arbol1 arbol2                    ==  False
-- ---------------------------------------------------------------------

mismaForma :: Arbol a -> Arbol b -> Bool
mismaForma (Hoja _)   (Hoja _)     = True
mismaForma (Nodo l r) (Nodo l' r') = mismaForma l l' && mismaForma r r'
mismaForma _          _            = False

-- ---------------------------------------------------------------------
-- Ejercicio 33. Definir (usando ==, mapArbol, const y ()) la función
--    mismaForma' :: Arbol a -> Arbol b -> Bool
-- tal que (mismaForma' t1 t2) se verifica si t1 y t2 tienen la misma
-- estructura. Por ejemplo,
--    mismaForma' arbol3 (mapArbol (* 10) arbol3)  ==  True
--    mismaForma' arbol1 arbol2                    ==  False
-- ---------------------------------------------------------------------

mismaForma' :: Arbol a -> Arbol b -> Bool
mismaForma' x y = f x == f y
  where
    f = mapArbol (const ())

-- ---------------------------------------------------------------------
-- Ejercicio 34. Definir el procedimiento
--    arbolArbitrario :: Arbitrary a => Int -> Gen (Arbol a)
-- tal que (arbolArbitrario n) es un árbol aleatorio de altura n. Por
-- ejemplo,
--    λ> sample (arbolArbitrario 3 :: Gen (Arbol Int))
--    Nodo (Nodo (Nodo (Hoja 0) (Hoja 0)) (Hoja 0)) (Hoja 0)
--    Nodo (Nodo (Hoja 1) (Hoja (-1))) (Hoja (-1))
--    --    Nodo (Nodo (Hoja 3) (Hoja 1)) (Hoja 4)
--    Nodo (Nodo (Hoja 4) (Hoja 8)) (Hoja (-4))
--    Nodo (Nodo (Nodo (Hoja 4) (Hoja 10)) (Hoja (-6))) (Hoja (-1))
--    Nodo (Nodo (Hoja 3) (Hoja 6)) (Hoja (-5))
--    Nodo (Nodo (Hoja (-11)) (Hoja (-13))) (Hoja 14)
--    Nodo (Nodo (Hoja (-7)) (Hoja 15)) (Hoja (-2))
--    Nodo (Nodo (Hoja (-9)) (Hoja (-2))) (Hoja (-6))
--    Nodo (Nodo (Hoja (-15)) (Hoja (-16))) (Hoja (-20))
-- ---------------------------------------------------------------------

arbolArbitrario :: Arbitrary a => Int -> Gen (Arbol a)
arbolArbitrario n
  | n <= 1    = Hoja <$> arbitrary
  | otherwise = do
      k <- choose (2, n - 1)
      Nodo <$> arbolArbitrario k <*> arbolArbitrario (n - k)

-- ---------------------------------------------------------------------
-- Ejercicio 35. Declarar Arbol como subclase de Arbitraria usando el
-- generador arbolArbitrario.
-- ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized arbolArbitrario
  shrink (Hoja x)   = Hoja <$> shrink x
  shrink (Nodo l r) = l :
                      r :
                      [Nodo l' r | l' <- shrink l] ++
                      [Nodo l r' | r' <- shrink r]

-- ---------------------------------------------------------------------
-- Ejercicio 36. Comprobar con QuickCheck que las funciones mismaForma y
-- mismaForma' son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mismaForma_mismaForma' :: Arbol Int -> Arbol Int -> Property
prop_mismaForma_mismaForma' a1 a2 =
  mismaForma a1 a2 === mismaForma' a1 a2

-- La comprobación es
--    λ> quickCheck prop_mismaForma_mismaForma'
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 37. Definir la función
--    creaArbol :: Int -> Arbol ()
-- tal que (creaArbol n) es el árbol cuyas hoyas están en la profundidad
-- n. Por ejemplo,
--    λ> creaArbol 2
--    Nodo (Nodo (Hoja ()) (Hoja ())) (Nodo (Hoja ()) (Hoja ()))
-- ---------------------------------------------------------------------

creaArbol :: Int -> Arbol ()
creaArbol h
  | h <= 0    = Hoja ()
  | otherwise = let x = creaArbol (h - 1) in Nodo x x

-- ---------------------------------------------------------------------
-- Ejercicio 38. Definir la función
--    injerta :: Arbol (Arbol a) -> Arbol a
-- tal que (injerta t) es el árbol obtenido sustituyendo cada hoja por el
-- árbol que contiene. Por ejemplo,
--    > injerta (Nodo (Hoja (Hoja 'x')) (Hoja (Nodo (Hoja 'y') (Hoja 'z'))))
--    Nodo (Hoja 'x') (Nodo (Hoja 'y') (Hoja 'z'))
-- ---------------------------------------------------------------------

injerta :: Arbol (Arbol a) -> Arbol a
injerta (Hoja t)   = t
injerta (Nodo l r) = Nodo (injerta l) (injerta r)

-- ---------------------------------------------------------------------
-- § Expresiones                                                      --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 39. Definir el tipo de las expresiones aritméticas formada
-- por
-- + literales (p.e. Lit 7),
-- + sumas (p.e. Suma (Lit 7) (Suma (Lit 3) (Lit 5)))
-- + opuestos (p.e. Op (Suma (Op (Lit 7)) (Suma (Lit 3) (Lit 5))))
-- + expresiones condicionales (p.e. (SiCero (Lit 3) (Lit 4) (Lit 5))
-- ---------------------------------------------------------------------

data Expr =
    Lit Int
  | Suma Expr Expr
  | Op Expr
  | SiCero Expr Expr Expr
  deriving (Eq, Show)

-- En los ejemplos se usarán las siguientes expresiones:
expr1, expr2 :: Expr
expr1 = Op (Suma (Lit 3) (Lit 5))
expr2 = SiCero expr1 (Lit 1) (Lit 0)

-- ---------------------------------------------------------------------
-- Ejercicio 40. Definir la función
--    valor :: Expr -> Int
-- tal que (valor e) es el valor de la expresión e (donde el valor de
-- (SiCero e e1 e2) es el valor de e1 si el valor de e es cero y el es
-- el valor de e2, en caso contrario). Por ejemplo,
--    valor expr1  ==  -8
--    valor expr2  ==  0
-- ---------------------------------------------------------------------

valor :: Expr -> Int
valor (Lit n)        = n
valor (Suma x y)     = valor x + valor y
valor (Op x)         = - valor x
valor (SiCero x y z) | valor x == 0 = valor y
                     | otherwise    = valor z

-- ---------------------------------------------------------------------
-- Ejercicio 41. Definir la función
--    resta :: Expr -> Expr -> Expr
-- tal que (resta e1 e2) es la expresión correspondiente a la diferencia
-- de e1 y e2. Por ejemplo,
--    resta (Lit 42) (Lit 2)  ==  Suma (Lit 42) (Op (Lit 2))
-- ---------------------------------------------------------------------

resta :: Expr -> Expr -> Expr
resta x y = Suma x (Op y)

-- ---------------------------------------------------------------------
-- Ejercicio 42. Definir el procedimiento
--    exprArbitraria :: Int -> Gen Expr
-- tal que (exprArbitraria n) es una expresión aleatoria de tamaño n. Por
-- ejemplo,
--    λ> sample (exprArbitraria 3)
--    Op (Op (Lit 0))
--    SiCero (Lit 0) (Lit (-2)) (Lit (-1))
--    Op (Suma (Lit 3) (Lit 0))
--    Op (Lit 5)
--    Op (Lit (-1))
--    Op (Op (Lit 9))
--    Suma (Lit (-12)) (Lit (-12))
--    Suma (Lit (-9)) (Lit 10)
--    Op (Suma (Lit 8) (Lit 15))
--    SiCero (Lit 16) (Lit 9) (Lit (-5))
--    Suma (Lit (-3)) (Lit 1)
-- ---------------------------------------------------------------------

exprArbitraria :: Int -> Gen Expr
exprArbitraria n
  | n <= 1 = Lit <$> arbitrary
  | otherwise = oneof
                [ Lit <$> arbitrary
                , let m = div n 2
                  in Suma <$> exprArbitraria m <*> exprArbitraria m
                , Op <$> exprArbitraria (n - 1)
                , let m = div n 3
                  in SiCero <$> exprArbitraria m
                            <*> exprArbitraria m
                            <*> exprArbitraria m
                ]

-- ---------------------------------------------------------------------
-- Ejercicio 43. Declarar Expr como subclase de Arbitraria usando el
-- generador exprArbitraria.
-- ---------------------------------------------------------------------

instance Arbitrary Expr where
  arbitrary = sized exprArbitraria

-- ---------------------------------------------------------------------
-- Ejercicio 44. Comprobar con QuickCheck que
--    valor (resta x y) == valor x - valor y
-- ---------------------------------------------------------------------

-- La propiedad es
prop_resta :: Expr -> Expr -> Property
prop_resta x y =
  valor (resta x y) === valor x - valor y

-- La comprobación es
--    λ> quickCheck prop_resta
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 45. Definir la función
--    numeroOps :: Expr -> Int
-- tal que (numeroOps e) es el número de operaciones de e. Por ejemplo,
--    numeroOps (Lit 3)                      ==  0
--    numeroOps (Suma (Lit 7) (Op (Lit 5)))  ==  2
-- ---------------------------------------------------------------------

numeroOps :: Expr -> Int
numeroOps (Lit _)        = 0
numeroOps (Suma x y)     = 1 + numeroOps x + numeroOps y
numeroOps (Op x)         = 1 + numeroOps x
numeroOps (SiCero x y z) = 1 + numeroOps x + numeroOps y + numeroOps z

-- ---------------------------------------------------------------------
-- Ejercicio 46. Definir la función
--    cadenaExpr :: Expr -> String
-- tal que (cadenaExpr e) es la cadena que representa la expresión e. Por
-- ejemplo,
--    λ> expr2
--    SiCero (Op (Suma (Lit 3) (Lit 5))) (Lit 1) (Lit 0)
--    λ> cadenaExpr expr2
--    "(if (- (3 + 5)) == 0 then 1 else 0)"
-- ---------------------------------------------------------------------

cadenaExpr :: Expr -> String
cadenaExpr (Lit n)
    | n >= 0              = show n
    | otherwise           = '(' : show n ++ ")"
cadenaExpr (Suma x y)     = '(' : cadenaExpr x ++ " + " ++ cadenaExpr y ++ ")"
cadenaExpr (Op x)         = "(- " ++ cadenaExpr x ++ ")"
cadenaExpr (SiCero x y z) = "(if " ++ cadenaExpr x ++ " == 0 then " ++
                                 cadenaExpr y ++ " else " ++
                                 cadenaExpr z ++ ")"

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Esta relación de ejercicios es una adaptación de la de Lars Brünjes
-- "Datatypes.hs" https://bit.ly/3sGYmYP
