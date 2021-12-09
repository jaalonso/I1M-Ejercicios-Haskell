-- Mayorias_parlamentarias.hs
-- Mayorías parlamentarias.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Mayorias_parlamentarias where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presenta un caso de estudio de los tipos
-- de datos algebraicos para estudiar las mayorías parlamentarias.
-- Además, con QuickCheck, se comprueban propiedades de las funciones
-- definidas.

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir el tipo de datos Partido para representar los
-- partidos de un Parlamento. Los partidos son P1, P2,..., P8. La clase
-- Partido está contenida en Eq, Ord y Show.
-- ---------------------------------------------------------------------

data Partido = P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir el tipo Parlamentarios para representar el
-- número de parlamentarios que posee un partido en el parlamento.
-- ---------------------------------------------------------------------

type Parlamentarios = Integer

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir el tipo (Tabla a b) para representar una lista
-- de pares de elementos el primero de tipo a y el segundo de tipo
-- b. Definir Asamblea para representar una tabla de partidos y
-- parlamentarios.
-- ---------------------------------------------------------------------

type Tabla a b = [(a,b)]
type Asamblea  = Tabla Partido Parlamentarios

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    partidos :: Asamblea -> [Partido]
-- tal que (partidos a) es la lista de partidos en la asamblea a. Por
-- ejemplo,
--    partidos [(P1,3),(P3,5),(P4,3)]  ==  [P1,P3,P4]
-- ---------------------------------------------------------------------

-- 1ª definición
partidos :: Asamblea -> [Partido]
partidos a = [p | (p,_) <- a]

-- 2ª definición
partidos2 :: Asamblea -> [Partido]
partidos2 = map fst

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    parlamentarios :: Asamblea -> Integer
-- tal que (parlamentarios a) es el número de parlamentarios en la
-- asamblea a. Por ejemplo,
--    parlamentarios [(P1,3),(P3,5),(P4,3)]  ==  11
-- ---------------------------------------------------------------------

-- 1ª definición
parlamentarios :: Asamblea -> Integer
parlamentarios a = sum [e | (_,e) <- a]

-- 2ª definición
parlamentarios2 :: Asamblea -> Integer
parlamentarios2 = sum . map snd

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    busca :: Eq a => a -> Tabla a b -> b
-- tal que (busca x t) es el valor correspondiente a x en la tabla
-- t. Por ejemplo,
--    λ> busca P3 [(P1,2),(P3,19)]
--    19
--    λ> busca P8 [(P1,2),(P3,19)]
--    *** Exception: no tiene valor en la tabla
-- ---------------------------------------------------------------------

-- 1ª solución (por comprensión)
busca :: Eq a => a -> Tabla a b -> b
busca x t | null xs   = error "no tiene valor en la tabla"
          | otherwise = head xs
  where xs = [b | (a,b) <- t, a == x]


-- 2ª definición (por recursión)
busca2 :: Eq a => a -> Tabla a b -> b
busca2 _ []            = error "no tiene valor en la tabla"
busca2 x ((x',y):xys)
  | x == x'         = y
  | otherwise       = busca2 x xys

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    busca' :: Eq a => a -> Table a b -> Maybe b
-- tal que (busca' x t) es justo el valor correspondiente a x en la
-- tabla t, o Nothing si x no tiene valor. Por ejemplo,
--    busca' P3 [(P1,2),(P3,19)]   ==   Just 19
--    busca' P8 [(P1,2),(P3,19)]   ==   Nothing
-- ---------------------------------------------------------------------

-- 1ª definición
busca' :: Eq a => a -> Tabla a b -> Maybe b
busca' x t | null xs   = Nothing
           | otherwise = Just (head xs)
  where xs = [b | (a,b) <- t, a == x]

-- 2ª definición
busca'2 :: Eq a => a -> Tabla a b -> Maybe b
busca'2 _ []          = Nothing
busca'2 x ((x',y):xys)
  | x == x'         = Just y
  | otherwise       = busca'2 x xys

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck que si (busca' x t) es
-- Nothing, entonces x es distinto de todos los elementos de t.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_BuscaNothing :: Integer -> [(Integer,Integer)] -> Property
prop_BuscaNothing x t =
  busca' x t == Nothing ==>
  x `notElem` [a | (a,_) <- t]

-- La comprobación es
--    λ> quickCheck prop_BuscaNothing
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 9. Comprobar que la función busca' es equivalente a la
-- función lookup del Prelude.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_BuscaEquivLookup :: Integer -> [(Integer,Integer)] -> Bool
prop_BuscaEquivLookup x t =
  busca' x t == lookup x t

-- La comprobación es
--    λ> quickCheck prop_BuscaEquivLookup
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir el tipo Coalicion como una lista de partidos.
-- ---------------------------------------------------------------------

type Coalicion = [Partido]

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    mayoria :: Asamblea -> Integer
-- tal que (mayoria xs) es el número de parlamentarios que se necesitan
-- para tener la mayoría en la asamblea xs. Por ejemplo,
--    mayoria [(P1,3),(P3,5),(P4,3)]   ==   6
--    mayoria [(P1,3),(P3,6)]          ==   5
-- ---------------------------------------------------------------------

mayoria :: Asamblea -> Integer
mayoria xs = parlamentarios xs `div` 2 + 1

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    coaliciones :: Asamblea -> Integer -> [Coalicion]
-- tal que (coaliciones xs n) es la lista de coaliciones necesarias para
-- alcanzar n parlamentarios. Por ejemplo,
--    coaliciones [(P1,3),(P2,2),(P3,1)] 3   ==  [[P2,P3],[P1]]
--    coaliciones [(P1,3),(P3,5),(P4,3)] 6   ==  [[P3,P4],[P1,P4],[P1,P3]]
--    coaliciones [(P1,3),(P3,5),(P4,3)] 9   ==  [[P1,P3,P4]]
--    coaliciones [(P1,3),(P3,5),(P4,3)] 14  ==  []
--    coaliciones [(P1,3),(P3,5),(P4,3)] 2   ==  [[P4],[P3],[P1]]
--    coaliciones [(P1,2),(P3,5),(P4,3)] 6   ==  [[P3,P4],[P1,P3]]
-- ---------------------------------------------------------------------

coaliciones :: Asamblea -> Integer -> [Coalicion]
coaliciones _ n | n <= 0  = [[]]
coaliciones [] _          = []
coaliciones ((p,m):xs) n  =
  coaliciones xs n ++ [p:c | c <- coaliciones xs (n-m)]

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    mayorias :: Asamblea -> [Coalicion]
-- tal que (mayorias a) es la lista de coaliciones mayoritarias en la
-- asamblea a. Por ejemplo,
--    mayorias [(P1,3),(P3,5),(P4,3)]   ==   [[P3,P4],[P1,P4],[P1,P3]]
--    mayorias [(P1,2),(P3,5),(P4,3)]   ==   [[P3,P4],[P1,P3]]
-- ---------------------------------------------------------------------

mayorias :: Asamblea -> [Coalicion]
mayorias asamblea =
  coaliciones asamblea (mayoria asamblea)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir el tipo de datos Asamblea.
-- ---------------------------------------------------------------------

data Asamblea2 = A Asamblea
  deriving Show

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la propiedad
--    esMayoritaria :: Coalicion -> Asamblea -> Bool
-- tal que (esMayoritaria c a) se verifica si la coalición c es
-- mayoritaria en la asamblea a. Por ejemplo,
--    esMayoritaria [P3,P4] [(P1,3),(P3,5),(P4,3)]   ==   True
--    esMayoritaria [P4] [(P1,3),(P3,5),(P4,3)]      ==   False
-- ---------------------------------------------------------------------

esMayoritaria :: Coalicion -> Asamblea -> Bool
esMayoritaria c a =
  sum [busca p a | p <- c] >= mayoria a

-- ---------------------------------------------------------------------
-- Ejercicio 16. Comprobar con QuickCheck que las coaliciones
-- obtenidas por (mayorias asamblea) son coaliciones mayoritarias en la
-- asamblea.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_MayoriasSonMayoritarias :: Asamblea2 -> Bool
prop_MayoriasSonMayoritarias (A asamblea) =
  and [esMayoritaria c asamblea | c <- mayorias asamblea]

-- La comprobación es
--    λ> quickCheck prop_MayoriasSonMayoritarias
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    esMayoritariaMinimal :: Coalicion -> Asamblea -> Bool
-- tal que (esMayoritariaMinimal c a) se verifica si la coalición c es
-- mayoritaria en la asamblea a, pero si se quita a c cualquiera de sus
-- partidos la coalición resultante no es mayoritaria. Por ejemplo,
--    esMayoritariaMinimal [P3,P4] [(P1,3),(P3,5),(P4,3)]     ==  True
--    esMayoritariaMinimal [P1,P3,P4] [(P1,3),(P3,5),(P4,3)]  ==  False
-- ---------------------------------------------------------------------

esMayoritariaMinimal :: Coalicion -> Asamblea -> Bool
esMayoritariaMinimal c a =
  esMayoritaria c a &&
  and [not(esMayoritaria (delete p c) a) | p <-c]

-- ---------------------------------------------------------------------
-- Ejercicio 18. Comprobar con QuickCheck si las coaliciones obtenidas
-- por (mayorias asamblea) son coaliciones mayoritarias minimales en la
-- asamblea.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_MayoriasSonMayoritariasMinimales :: Asamblea2 -> Bool
prop_MayoriasSonMayoritariasMinimales (A asamblea) =
  and [esMayoritariaMinimal c asamblea | c <- mayorias asamblea]

-- La comprobación es
--    λ> quickCheck prop_MayoriasSonMayoritariasMinimales
--    Falsifiable, after 0 tests:
--    A [(P1,1),(P2,0),(P3,1),(P4,1),(P5,0),(P6,1),(P7,0),(P8,1)]

-- Por tanto, no se cumple la propiedad. Para buscar una coalición no
-- minimal generada por mayorias, definimos la función
contraejemplo :: Asamblea -> Coalicion
contraejemplo a =
  head [c | c <- mayorias a, not(esMayoritariaMinimal c a)]

-- El cálculo del contraejemplo es
--    λ> contraejemplo [(P1,1),(P2,0),(P3,1),(P4,1),(P5,0),(P6,1),(P7,0),(P8,1)]
--    [P4,P6,P7,P8]

-- La coalición [P4,P6,P7,P8] no es minimal ya que [P4,P6,P8] también es
-- mayoritaria. En efecto,
--    λ> esMayoritaria [P4,P6,P8]
--                        [(P1,1),(P2,0),(P3,1),(P4,1),
--                         (P5,0),(P6,1),(P7,0),(P8,1)]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    coalicionesMinimales :: Asamblea -> Integer -> [Coalicion,Parlamentarios]
-- tal que (coalicionesMinimales xs n) es la lista de coaliciones
-- minimales necesarias para alcanzar n parlamentarios. Por ejemplo,
--    λ> coalicionesMinimales [(P1,3),(P3,5),(P4,3)] 6
--    [([P3,P4],8),([P1,P4],6),([P1,P3],8)]
--    λ> coalicionesMinimales [(P1,3),(P3,5),(P4,3)] 5
--    [([P3],5),([P1,P4],6)]
-- ---------------------------------------------------------------------

coalicionesMinimales :: Asamblea -> Integer -> [(Coalicion,Parlamentarios)]
coalicionesMinimales _ n | n <= 0  = [([],0)]
coalicionesMinimales [] _          = []
coalicionesMinimales ((p,m):xs) n  =
  coalicionesMinimales xs n ++
  [(p:ys, t+m) | (ys,t) <- coalicionesMinimales xs (n-m), t<n]

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    mayoriasMinimales :: Asamblea -> [Coalicion]
-- tal que (mayoriasMinimales a) es la lista de coaliciones mayoritarias
-- minimales en la asamblea a. Por ejemplo,
--    mayoriasMinimales [(P1,3),(P3,5),(P4,3)] == [[P3,P4],[P1,P4],[P1,P3]]
-- ---------------------------------------------------------------------

mayoriasMinimales :: Asamblea -> [Coalicion]
mayoriasMinimales asamblea =
  [c | (c,_) <- coalicionesMinimales asamblea (mayoria asamblea)]

-- ---------------------------------------------------------------------
-- Ejercicio 21. Comprobar con QuickCheck que las coaliciones
-- obtenidas por (mayoriasMinimales asamblea) son coaliciones
-- mayoritarias minimales en la asamblea.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_MayoriasMinimalesSonMayoritariasMinimales :: Asamblea2 -> Bool
prop_MayoriasMinimalesSonMayoritariasMinimales (A asamblea) =
  and [esMayoritariaMinimal c asamblea
       | c <- mayoriasMinimales asamblea]

-- La comprobación es
--    λ> quickCheck prop_MayoriasMinimalesSonMayoritariasMinimales
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Funciones auxiliares                                               --
-- ---------------------------------------------------------------------

-- (listaDe n g) es una lista de n elementos, donde cada elemento es
-- generado por g. Por ejemplo,
--    λ> muestra (listaDe 3 (arbitrary :: Gen Int))
--    [-1,1,-1]
--    [-2,-4,-1]
--    [1,-1,0]
--    [1,-1,1]
--    [1,-1,1]
--    λ> muestra (listaDe 3 (arbitrary :: Gen Bool))
--    [False,True,False]
--    [True,True,False]
--    [False,False,True]
--    [False,False,True]
--    [True,False,True]
listaDe :: Int -> Gen a -> Gen [a]
listaDe n g = sequence [g | _ <- [1..n]]

-- paresDeIgualLongitud genera pares de listas de igual longitud. Por
-- ejemplo,
--    λ> muestra (paresDeIgualLongitud (arbitrary :: Gen Int))
--    ([-4,5],[-4,2])
--    ([],[])
--    ([0,0],[-2,-3])
--    ([2,-2],[-2,1])
--    ([0],[-1])
--    λ> muestra (paresDeIgualLongitud (arbitrary :: Gen Bool))
--    ([False,True,False],[True,True,True])
--    ([True],[True])
--    ([],[])
--    ([False],[False])
--    ([],[])
paresDeIgualLongitud :: Gen a -> Gen ([a],[a])
paresDeIgualLongitud gen = do
  n <- arbitrary
  xs <- listaDe (abs n) gen
  ys <- listaDe (abs n) gen
  return (xs,ys)

-- generaAsamblea esun generador de datos de tipo Asamblea. Por ejemplo,
--    λ> muestra generaAsamblea
--    A [(P1,1),(P2,1),(P3,0),(P4,1),(P5,0),(P6,1),(P7,0),(P8,1)]
--    A [(P1,0),(P2,1),(P3,1),(P4,1),(P5,0),(P6,1),(P7,0),(P8,1)]
--    A [(P1,1),(P2,2),(P3,0),(P4,1),(P5,0),(P6,1),(P7,2),(P8,0)]
--    A [(P1,1),(P2,0),(P3,1),(P4,0),(P5,0),(P6,1),(P7,1),(P8,1)]
--    A [(P1,1),(P2,0),(P3,0),(P4,0),(P5,1),(P6,1),(P7,1),(P8,0)]
generaAsamblea :: Gen Asamblea2
generaAsamblea = do
  xs <- listaDe 8 (arbitrary :: Gen Integer)
  return (A (zip [P1,P2,P3,P4,P5,P6,P7,P8] (map abs xs)))

instance Arbitrary Asamblea2 where
  arbitrary   = generaAsamblea
  -- coarbitrary = undefined
