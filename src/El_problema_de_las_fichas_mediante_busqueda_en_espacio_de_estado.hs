-- El_problema_de_las_fichas_mediante_busqueda_en_espacio_de_estado.hs
-- El problema de las fichas mediante búsqueda en espacio de estado.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module El_problema_de_las_fichas_mediante_busqueda_en_espacio_de_estado where

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- Para el problema de las fichas de orden (m,n) se considera un tablero
-- con m+n+1 cuadrados consecutivos.
--
-- Inicialmente, en cada uno de los m primeros cuadrados hay una ficha
-- blanca, a continuación un hueco y  en cada uno de los n últimos
-- cuadrados hay una ficha verde. El objetivo consiste en tener las
-- fichas verdes al principio y las blancas al final.
--
-- Por ejemplo, en el problema de las fichas de orden (3,3) la situación
-- inicial es
--       +---+---+---+---+---+---+---+
--       | B | B | B |   | V | V | V |
--       +---+---+---+---+---+---+---+
-- y la final es
--       +---+---+---+---+---+---+---+
--       | V | V | V |   | B | B | B |
--       +---+---+---+---+---+---+---+
--
-- Los movimientos permitidos consisten en desplazar una ficha al hueco
-- saltando, como máximo, sobre otras dos.
--
-- El objetivo de esta relación de ejercicios es resolver el problema
-- de las fichas mediante búsqueda en espacio de estados, utilizando las
-- implementaciones estudiadas en el tema 23
--    https://jaalonso.github.io/cursos/i1m/temas/tema-23.html
--
-- Para realizar los ejercicios hay que tener instalada la librería de
-- I1M. Para instalarla basta ejecutar en una consola
--    cabal update
--    cabal install I1M

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

import I1M.BusquedaEnEspaciosDeEstados
import I1M.BusquedaPrimeroElMejor
import I1M.BusquedaEnEscalada
import I1M.Cola

-- ---------------------------------------------------------------------
-- § Representación de estados                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir el tipo Ficha con tres constructores B, V y H
-- que representan las fichas blanca, verde y hueco, respectivamente.
-- ---------------------------------------------------------------------

data Ficha = B | V | H
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir el tipo Estado como abreviatura de una lista de
-- fichas que representa las fichas colocadas en el tablero.
-- ---------------------------------------------------------------------

type Estado = [Ficha]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    inicial ::  Int -> Int -> Estado
-- tal que (inicial m n) representa el estado inicial del problema de
-- las fichas de orden (m,n). Por ejemplo,
--    inicial 2 3  ==  [B,B,H,V,V,V]
--    inicial 3 2  ==  [B,B,B,H,V,V]
-- ---------------------------------------------------------------------

inicial ::  Int -> Int -> Estado
inicial m n = replicate m B ++ [H] ++ replicate n V

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    final ::  Int -> Int -> Estado
-- tal que (final m n) representa el estado final del problema de
-- las fichas de orden (m,n). Por ejemplo,
--    final 2 3  ==  [V,V,V,H,B,B]
--    final 3 2  ==  [V,V,H,B,B,B]
-- ---------------------------------------------------------------------

final ::  Int -> Int -> Estado
final m n = replicate n V ++ [H] ++ replicate m B

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    sucesoresE :: Estado -> [Estado]
-- tal que (sucesoresE e) es la lista de los sucesores del estado e. Por
-- ejemplo,
--    λ> sucesoresE [V,B,H,V,V,B]
--    [[V,H,B,V,V,B],[H,B,V,V,V,B],[V,B,V,H,V,B],[V,B,V,V,H,B],
--     [V,B,B,V,V,H]]
--    λ> sucesoresE [B,B,B,H,V,V,V]
--    [[B,B,H,B,V,V,V],[B,H,B,B,V,V,V],[H,B,B,B,V,V,V],
--     [B,B,B,V,H,V,V],[B,B,B,V,V,H,V],[B,B,B,V,V,V,H]]
-- ---------------------------------------------------------------------

sucesoresE :: Estado -> [Estado]
sucesoresE e =
  [intercambia i j e | i <- [j-1,j-2,j-3,j+1,j+2,j+3]
                     , 0 <= i, i < n]
  where j = posicionHueco e
        n = length e

-- (posicionHueco e) es la posición del hueco en el estado e. Por
-- ejemplo,
--    posicionHueco inicial  ==  3
posicionHueco :: Estado -> Int
posicionHueco e = length (takeWhile (/=H) e)

-- (intercambia xs i j) es la lista obtenida intercambiando los
-- elementos de xs en las posiciones i y j. Por ejemplo,
--    intercambia 2 6 [0..9]  ==  [0,1,6,3,4,5,2,7,8,9]
--    intercambia 6 2 [0..9]  ==  [0,1,6,3,4,5,2,7,8,9]
intercambia :: Int -> Int -> [a] -> [a]
intercambia i j xs = concat [xs1,[x2],xs2,[x1],xs3]
  where (xs1,x1,xs2,x2,xs3) = divide (min i j) (max i j) xs

-- (divide xs i j) es la tupla (xs1,x1,xs2,x2,xs3) tal que xs1 son los
-- elementos de xs cuya posición es menos que i, x1 es el elemento de xs
-- en la posición i, xs2 son los elementos de xs cuya posición es mayor
-- que i y menor que j, x2 es el elemento de xs en la posición j y xs3
-- son los elementos de xs cuya posición es mayor que j (suponiendo que
-- i < j). Por ejemplo,
--    divide 2 6 [0..9]  ==  ([0,1],2,[3,4,5],6,[7,8,9])
divide :: Int -> Int -> [a] -> ([a],a,[a],a,[a])
divide i j xs = (xs1,x1,xs2,x2,xs3)
  where (xs1,x1:ys)  = splitAt i xs
        (xs2,x2:xs3) = splitAt (j - i - 1) ys

-- ---------------------------------------------------------------------
-- Ejercicio 6. Los nodos del espacio de búsqueda son lista de estados
--    [e_n, ..., e_2, e_1]
-- donde e_1 es el estado inicial y para cada i (2 <= i <= n), e_i es un
-- sucesor de e_(i-1).
--
-- Definir el tipo de datos Nodo para representar los nodos del
-- espacio de búsqueda. Por ejemplo,
--    λ> :type (N [[B,H,B,V,V,V],[B,B,H,V,V,V]])
--    (N [[B,H,B,V,V,V],[B,B,H,V,V,V]]) :: Nodo
-- ---------------------------------------------------------------------

newtype Nodo = N [Estado]
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    inicialN ::  Int -> Int -> Nodo
-- tal que (inicialN m n) representa el nodo inicial del problema de
-- las fichas de orden (m,n). Por ejemplo,
--    inicialN 2 3  ==  N [[B,B,H,V,V,V]]
--    inicialN 3 2  ==  N [[B,B,B,H,V,V]]
-- ---------------------------------------------------------------------

inicialN :: Int -> Int -> Nodo
inicialN m n = N [inicial m n]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    esFinalN :: Int -> Int -> Nodo -> Bool
-- tal que (esFinalN m n) se verifica si N es un nodo final del problema
-- de las fichas de orden (m,n). Por ejemplo,
--    λ> esFinalN 2 1 (N [[V,H,B,B],[V,B,B,H],[H,B,B,V],[B,B,H,V]])
--    True
--    λ> esFinalN 2 1 (N [[V,B,B,H],[H,B,B,V],[B,B,H,V]])
--    False
-- ---------------------------------------------------------------------

esFinalN :: Int -> Int -> Nodo -> Bool
esFinalN m n (N (e:_)) = e == final m n
esFinalN _ _ (N [])    = error "Imposible"

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    sucesoresN :: Nodo -> [Nodo]
-- tal que (sucesoresN n) es la lista de los sucesores del nodo n. Por
-- ejemplo,
--    λ> sucesoresN (N [[H,B,B,V],[B,B,H,V]])
--    [N [[B,H,B,V],[H,B,B,V],[B,B,H,V]],
--     N [[V,B,B,H],[H,B,B,V],[B,B,H,V]]]
--    λ> sucesoresN (N [[B,H,B,V],[H,B,B,V],[B,B,H,V]])
--    [N [[B,V,B,H],[B,H,B,V],[H,B,B,V],[B,B,H,V]]]
-- ---------------------------------------------------------------------

sucesoresN :: Nodo -> [Nodo]
sucesoresN (N n@(e:es)) =
  [N (e':n) | e' <- sucesoresE e,
              e' `notElem` es]
sucesoresN (N []) = error "Imposible"

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    solucionesEE :: Int -> Int -> [[Estado]]
-- tal que (solucionesEE m n) es la lista de las soluciones del problema
-- de las dichas obtenidas con el patrón buscaEE (que realiza la
-- búsqueda en profundidad). Por ejemplo,
--    λ> mapM_ print (zip [0..] (head (solucionesEE 2 2)))
--    ( 0,[B,B,H,V,V])
--    ( 1,[B,H,B,V,V])
--    ( 2,[H,B,B,V,V])
--    ( 3,[V,B,B,H,V])
--    ( 4,[V,B,H,B,V])
--    ( 5,[V,H,B,B,V])
--    ( 6,[H,V,B,B,V])
--    ( 7,[B,V,H,B,V])
--    ( 8,[B,H,V,B,V])
--    ( 9,[H,B,V,B,V])
--    (10,[B,B,V,H,V])
--    (11,[B,B,V,V,H])
--    (12,[B,H,V,V,B])
--    (13,[H,B,V,V,B])
--    (14,[V,B,H,V,B])
--    (15,[V,H,B,V,B])
--    (16,[H,V,B,V,B])
--    (17,[B,V,H,V,B])
--    (18,[B,V,V,H,B])
--    (19,[H,V,V,B,B])
--    (20,[V,H,V,B,B])
--    (21,[V,V,H,B,B])
--
--    λ> length (head (solucionesEE 6 5))
--    2564
--    (13.65 secs, 256,880,520 bytes)
-- ---------------------------------------------------------------------

solucionesEE :: Int -> Int -> [[Estado]]
solucionesEE m n =
  [reverse es | N es <- buscaEE sucesoresN (esFinalN  m n) (inicialN m n)]

-- ---------------------------------------------------------------------
-- Ejercicio 11. Se considera la heurística que para cada estado vale la
-- suma de piezas blancas situadas a la izquierda de cada una de las
-- piezas verdes. Por ejemplo, para el estado
--       +---+---+---+---+---+---+---+
--       | B | V | B |   | V | V | B |
--       +---+---+---+---+---+---+---+
-- su valor es 1+2+2 = 5.
--
-- Definir la función
--    heuristicaE :: Estado -> Int
-- tal que (heuristicaE e) es la heurística del estado e. Por ejemplo,
--    heuristicaE [B,V,B,H,V,V,B] == 5
-- ---------------------------------------------------------------------

heuristicaE :: Estado -> Int
heuristicaE []     = 0
heuristicaE (V:xs) = heuristicaE xs
heuristicaE (H:xs) = heuristicaE xs
heuristicaE (B:xs) = heuristicaE xs + length (filter (==V) xs)

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    heuristicaN :: Nodo -> Int
-- tal que (heuristicaN n) es la heurística del primer estado del
-- camino. Por ejemplo,
--    heuristicaN (N [[H,B,B,V],[B,B,H,V]])            ==  2
--    heuristicaN (N [[V,B,B,H],[H,B,B,V],[B,B,H,V]])  ==  0
-- ---------------------------------------------------------------------

heuristicaN :: Nodo -> Int
heuristicaN (N (e:_)) = heuristicaE e
heuristicaN (N [])    = error "Imposible"

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la pertenencia de Nodo a Ord que forma que un
-- nodo es menor o igual que otro si su heurística lo es.
-- ---------------------------------------------------------------------

instance Ord Nodo where
  n1 <= n2 = heuristicaN n1 <= heuristicaN n2

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    solucionesPM :: Int -> Int -> [[Estado]]
-- tal que (solucionesPM m n) es la lista de las soluciones del problema
-- de las dichas obtenidas con el patrón buscaPM (que realiza la
-- búsqueda por primero el mejor). Por ejemplo,
--    λ> mapM_ print (zip [0..] (head (solucionesPM 2 2)))
--    (0,[B,B,H,V,V])
--    (1,[B,H,B,V,V])
--    (2,[B,V,B,H,V])
--    (3,[H,V,B,B,V])
--    (4,[V,H,B,B,V])
--    (5,[V,V,B,B,H])
--    (6,[V,V,B,H,B])
--    (7,[V,V,H,B,B])
--
--    λ> length (head (solucionesPM 6 5))
--    54
--    (0.05 secs, 5,430,056 bytes)
-- ---------------------------------------------------------------------

solucionesPM :: Int -> Int -> [[Estado]]
solucionesPM m n =
  [reverse es | N es <- buscaPM sucesoresN
                                (esFinalN m n)
                                (inicialN m n)]

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    solucionesEscalada :: Int -> Int -> [[Estado]]
-- tal que (solucionesEscalada m n) es la lista de las soluciones del
-- problema de las dichas obtenidas con el patrón buscaEscalada (que
-- realiza la búsqueda por escalada). Por ejemplo,
--    λ> mapM_ print (zip [0..] (head (solucionesEscalada 2 2)))
--    (0,[B,B,H,V,V])
--    (1,[B,H,B,V,V])
--    (2,[B,V,B,H,V])
--    (3,[H,V,B,B,V])
--    (4,[V,H,B,B,V])
--    (5,[V,V,B,B,H])
--    (6,[V,V,B,H,B])
--    (7,[V,V,H,B,B])
--
--    λ> length (head (solucionesEscalada 4 5))
--    37
--    (0.02 secs, 1,718,560 bytes)
--
--    λ> length (head (solucionesEscalada 6 5))
--    *** Exception: Prelude.head: empty list
-- ---------------------------------------------------------------------

solucionesEscalada :: Int -> Int -> [[Estado]]
solucionesEscalada m n =
  [reverse es | N es <- buscaEscalada sucesoresN
                                      (esFinalN m n)
                                      (inicialN m n)]

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    buscaAnchura :: (Eq nodo) =>
--                    (nodo -> [nodo])
--                 -> (nodo -> Bool)
--                 -> nodo
--                 -> [nodo]
-- tal que (buscaAnchura s o e) es la lista de soluciones del problema
-- de espacio de estado definido por la función sucesores (s), el
-- objetivo (o) y el estado inicial (e) mediante búsqueda en anchura.
-- ---------------------------------------------------------------------

buscaAnchura :: (Eq nodo) =>
                (nodo -> [nodo]) -- sucesores
             -> (nodo -> Bool)   -- esFinal
             -> nodo             -- nodo actual
             -> [nodo]           -- soluciones
buscaAnchura sucesores esFinal x = busca' (inserta x vacia)
 where
   busca' p
    | esVacia p = []
    | esFinal y = y : busca' (resto p)
    | otherwise = busca' (foldr inserta (resto p) (sucesores y))
     where y = primero p

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    solucionesAnchura :: Int -> Int -> [[Estado]]
-- tal que (solucionesAnchura m n) es la lista de las soluciones del problema
-- de las dichas obtenidas con el patrón buscaAnchura (que realiza la
-- búsqueda en profundidad). Por ejemplo,
--    λ> mapM_ print (zip [0..] (head (solucionesAnchura 2 2)))
--    (0,[B,B,H,V,V])
--    (1,[B,B,V,V,H])
--    (2,[B,H,V,V,B])
--    (3,[B,V,V,H,B])
--    (4,[H,V,V,B,B])
--    (5,[V,V,H,B,B])
--
--    λ> length (head (solucionesAnchura 3 2))
--    8
--    (0.22 secs, 100,912,336 bytes)
--    λ> length (head (solucionesEE 3 2))
--    37
--    (0.01 secs, 878,992 bytes)
--    λ> length (head (solucionesPM 3 2))
--    11
--    (0.01 secs, 826,824 bytes)
--
--    λ> import System.Timeout
--    (0.00 secs, 0 bytes)
--    λ> timeout (2*10^6) (return $! length (head (solucionesAnchura 3 3)))
--    Nothing
--    (2.03 secs, 1,246,161,256 bytes)
--    λ> timeout (10*10^6) (return $! length (head (solucionesAnchura 3 3)))
--    Nothing
--    (9.91 secs, 4,846,262,912 bytes)
--    λ> timeout (10*10^6) (return $! length (head (solucionesEE 3 3)))
--    Just 82
--    (0.04 secs, 2,649,472 bytes)
--    λ> timeout (10*10^6) (return $! length (head (solucionesPM 3 3)))
--    Just 18
--    (0.01 secs, 1,051,912 bytes)
-- ---------------------------------------------------------------------

solucionesAnchura :: Int -> Int -> [[Estado]]
solucionesAnchura m n =
  [reverse es | N es <- buscaAnchura sucesoresN (esFinalN  m n) (inicialN m n)]
