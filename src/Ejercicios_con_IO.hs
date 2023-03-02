-- Ejercicios_con_IO.hs
-- Ejercicios con IO (entrada/salida).
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-abril-2022
-- ---------------------------------------------------------------------

module Ejercicios_con_IO where

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es practicar con las
-- acciones IO (de entrada/salida) estudiadas en el tema 13 que se
-- encuentra en
--    https://jaalonso.github.io/materias/PFconHaskell/temas/tema-13.html
--
-- Concretamente, funciones para leer y escribir desde los dispositivos
-- estándar y desde ficheros, así como funciones con números aleatorios.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Prelude
import Control.Monad (liftM2)
import System.Random (randomRIO)
import Text.Read     (readMaybe)

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    leeEntero1 :: String -> Int
-- tal que (leeEntero1 cs) es el entero correspondiente a la cadena
-- cs. Por ejemplo,
--    λ> leeEntero1 "325"
--    325
--    λ> leeEntero1 "-25"
--    -25
--    λ> leeEntero1 "3.25"
--    *** Exception: Prelude.read: no parse
-- ---------------------------------------------------------------------

leeEntero1 :: String -> Int
leeEntero1 = read

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    leeEntero :: String -> Maybe Int
-- tal que (leeEntero1 cs) es justo el entero correspondiente a la
-- cadena cs, si representa un entero y Nothing en caso contrario. Por
-- ejemplo,
--    λ> leeEntero "325"
--    Just 325
--    λ> leeEntero "-25"
--    Just (-25)
--    λ> leeEntero "3.25"
--    Nothing
-- ---------------------------------------------------------------------

leeEntero :: String -> Maybe Int
leeEntero = readMaybe

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    leeLinea :: Read a => IO (Maybe a)
-- que lee una línea y devuelve el término del tipo indicado, si lo es o
-- Nothing en caso contrario. Por ejemplo,
--    λ> leeLinea :: IO (Maybe Int)
--    325
--    Just 325
--    λ> leeLinea :: IO (Maybe Int)
--    3.25
--    Nothing
--    λ> leeLinea :: IO (Maybe Float)
--    3.25
--    Just 3.25
-- ---------------------------------------------------------------------

leeLinea :: Read a => IO (Maybe a)
leeLinea = fmap readMaybe getLine

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función
--    sumaDosNumeros :: IO ()
-- que lea dos números y devuelva su suma. Por ejemplo,
--    λ> sumaDosNumeros
--    Escribe el primer numero:
--    2
--    Escribe el segundo numero:
--    3
--    La suma de los dos numeros es 5.
-- ---------------------------------------------------------------------

sumaDosNumeros :: IO ()
sumaDosNumeros = do
  putStrLn "Escribe el primer numero:"
  x <- readLn
  putStrLn "Escribe el segundo numero:"
  y <- readLn
  putStrLn $ "La suma de los dos numeros es " ++ show (x + y :: Int) ++ "."

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    replicateM :: Int -> IO a -> IO [a]
-- tal que (replicateM n a) repite n veces la acción a. Por ejemplo,
--    λ> replicateM 2 (leeLinea :: IO (Maybe Int))
--    325
--    3.25
--    [Just 325,Nothing]
-- ---------------------------------------------------------------------

replicateM :: Int -> IO a -> IO [a]
replicateM n a
  | n <= 0    = return []
  | otherwise = do
      x  <- a
      xs <- replicateM (n - 1) a
      return (x : xs)

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir la acción
--    sumaVarios :: IO ()
-- que pregunte por la cantidad de números a sumar, los leas e imprima
-- el resultado de su suma. Por ejemplo,
--    λ> sumaVarios
--    Escribe la cantidad de numeros a sumar
--    3
--    Escribe el siguiente numero:
--    2
--    Escribe el siguiente numero:
--    4
--    Escribe el siguiente numero:
--    5
--    La suma de todos los numeros es 11.
-- ---------------------------------------------------------------------

sumaVarios :: IO ()
sumaVarios = do
  putStrLn "Escribe la cantidad de numeros a sumar"
  n  <- readLn
  xs <- replicateM n (putStrLn "Escribe el siguiente numero:" >> readLn) :: IO [Int]
  putStrLn ("La suma de todos los numeros es " ++ show (sum xs) ++ ".")

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Definir la acción
--    sumaVarios' :: IO ()
-- que es una variante de la anterior pero indica el progreso de los
-- números. Por ejemplo,
--    λ> sumaVarios'
--    Escribe la cantidad de numeros a sumar
--    3
--    Escribe el número 1 de 3:
--    2
--    Escribe el número 2 de 3:
--    4
--    Escribe el número 3 de 3:
--    5
--    La suma de todos los numeros es 11.
-- ---------------------------------------------------------------------

sumaVarios' :: IO ()
sumaVarios' = do
  putStrLn "Escribe la cantidad de numeros a sumar"
  n  <- readLn
  xs <- mapM (\i -> putStrLn ("Escribe el número " ++ show i ++ " de " ++ show n ++ ":") >> readLn)
             [1 .. n :: Int] :: IO [Int]
  putStrLn ("La suma de todos los numeros es " ++ show (sum xs) ++ ".")

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir el procedimiento
--    wc :: FilePath -> IO (Int, Int, Int)
-- tal que (wc f) lle el contenido del fichero f y devuelve una terna
-- formada por sus números de filas, palabras y caracteres. Por ejemplo,
-- si el contenido del fichero /tmp/ejemplo1.txt es
--    Esta es la primera fila
--    esta es la segunda
--    y esta es la última.
-- entonces
--    λ> wc "/tmp/ejemplo1.txt"
--    (3,14,64)
-- ---------------------------------------------------------------------

wc :: FilePath -> IO (Int, Int, Int)
wc f = do
  s <- readFile f
  return (length (lines s), length (words s), length s)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir el procedimiento
--    dosDados :: IO (Int, Int)
-- que devuelva un par de números aleatorios que representan los valores
-- de dos dados. Por ejemplo,
--    λ> dosDados
--    (1,2)
--    λ> dosDados
--    (4,5)
-- ---------------------------------------------------------------------

dosDados :: IO (Int, Int)
dosDados = liftM2 (,) dado dado
  where
    dado = randomRIO (1, 6)

-- 2ª definición
dosDados2 :: IO (Int, Int)
dosDados2 = (,) <$> dado <*> dado
  where
    dado = randomRIO (1, 6)

-- 3ª definición
dosDados3 :: IO (Int, Int)
dosDados3 = do
  d1 <- randomRIO (1, 6)
  d2 <- randomRIO (1, 6)
  return (d1, d2)

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Una expresión de la forma
--    2d8 + 4
-- se interpreta como lanzar dos dos de 8 cara y sumarle 4. Se puede
-- representar mediante la expresión
--    2 `D` 8 `Mas` Const 4
-- usando el tipo datos ExpDado definido por
--    data ExpDado = D Int Int
--                 | Const Int
--                 | Mas ExpDado ExpDado
--      deriving (Show, Eq)
-- y declarando los siguientes operadores infijos
--    infix  7 `D`
--    infixl 6 `Mas`
--
-- Definir el procedimiento
--    valor :: ExpDado -> IO Int
-- tal que (valor e) devuelve el valor de la expresión e. Por
-- ejemplo,
--    λ> valor (2 `D` 8 `Mas` Const 4)
--    8
--    λ> valor (2 `D` 8 `Mas` Const 4)
--    14
--    λ> valor (3 `D` 6)
--    10
--    λ> valor (2 `D` 6 `Mas` 1 `D` 8)
--    15
--    λ> valor (2 `D` 6 `Mas` 1 `D` 8)
--    18
-- ---------------------------------------------------------------------


data ExpDado = D Int Int
             | Const Int
             | Mas ExpDado ExpDado
  deriving (Show, Eq)

infix  7 `D`
infixl 6 `Mas`

valor :: ExpDado -> IO Int
valor (D c d)
  | c <= 0    = return 0
  | d <= 0    = return 0
  | otherwise = sum <$> replicateM c (randomRIO (1, d))
valor (Const i) = return i
valor (Mas x y) = (+) <$> valor x <*> valor y

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función
--    rango :: ExpDado -> (Int, Int)
-- tal que (rango e) es el par formado por el mínimo y máximo de los
-- posibles valores de la expresión e. Por ejemplo,
--    λ> rango (2 `D` 6 `Mas` 1 `D` 8)
--    (3,20)
-- ---------------------------------------------------------------------

rango :: ExpDado -> (Int, Int)
rango (D c d)
  | c <= 0    = (0, 0)
  | d <= 0    = (0, 0)
  | otherwise = (c, c * d)
rango (Const i) = (i, i)
rango (Mas x y) = (l1 + l2, u1 + u2)
  where (l1, u1) = rango x
        (l2, u2) = rango y

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. La baraja inglesa consta de 4 palos (picas, corazones,
-- rombos y tréboles) y cada palo está formado por 13 cartas (A ,2, 3,
-- 4, 5, 6, 7, 8, 9, 10, J, Q y K).
--
-- La baraja se puede representar mediante los siguientes tipos de
-- datos:
--    data Palo = Picas | Corazones | Rombos | Treboles
--      deriving (Show, Eq, Bounded, Enum)
--
--    data NumeroCarta = A | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K
--      deriving (Show, Eq, Bounded, Enum)
--
--    data Carta = Carta NumeroCarta Palo
--      deriving (Show, Eq)
--
-- Definir la lista
--    cartas :: [Carta]
-- cuyos elementos son todas las cartas. Por ejemplo,
--    λ> take 26 cartas
--    [Carta A  Picas,Carta A  Corazones,Carta A  Rombos,Carta A  Treboles,
--     Carta C2 Picas,Carta C2 Corazones,Carta C2 Rombos,Carta C2 Treboles,
--     Carta C3 Picas,Carta C3 Corazones,Carta C3 Rombos,Carta C3 Treboles]
-- ---------------------------------------------------------------------

data Palo = Picas | Corazones | Rombos | Treboles
  deriving (Show, Eq, Bounded, Enum)

data NumeroCarta = A | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K
  deriving (Show, Eq, Bounded, Enum)

data Carta = Carta NumeroCarta Palo
  deriving (Show, Eq)

cartas :: [Carta]
cartas = [Carta n p | n <- [minBound ..], p <- [minBound ..]]

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir la función
--    extrae :: Int -> [a] -> (a, [a])
-- tal que (extrae i xs) es el par formado por el elemento i-ésimo de xs
-- y los restantes elementos. Por ejemplo,
--    λ> extrae 1 [3,5,4]
--    (5,[3,4])
--    λ> extrae 7 [3,5,4]
--    (*** Exception: indice fuera de ranfo
-- ---------------------------------------------------------------------

extrae :: Int -> [a] -> (a, [a])
extrae _ []       = error "indice fuera de ranfo"
extrae 0 (x : xs) = (x, xs)
extrae i (x : xs) = (y, x : ys)
  where (y, ys) = extrae (i - 1) xs

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir la función
--    permutacion :: [a] -> IO [a]
-- tal que (permutacion xs) es una permutación aleatoria de xs. Por
-- ejemplo,
--    λ> permutacion [1..6]
--    [5,4,3,1,6,2]
--    λ> permutacion [1..6]
--    [3,4,6,2,1,5]
-- ---------------------------------------------------------------------

permutacion :: [a] -> IO [a]
permutacion [] = return []
permutacion xs = do
  i <- randomRIO (0, length xs - 1)
  let (y, ys) = extrae i xs
  zs <- permutacion ys
  return (y : zs)

-- ---------------------------------------------------------------------
-- Ejercicio 6.4. Definir el procedimiento
--    cartasBarajadas :: Int -> IO [Carta]
-- tal que (cartasBarajadas n) que baraja las cartas y devuelve la lista
-- formada por las n primeras de ellas. Por ejemplo,
--    λ> cartasBarajadas 3
--    [Carta C3 Treboles,Carta C7 Picas,Carta C6 Treboles]
--    λ> cartasBarajadas 3
--    [Carta C10 Corazones,Carta C10 Treboles,Carta A Corazones]
--    λ> cartasBarajadas 3
--    [Carta C3 Rombos,Carta C2 Rombos,Carta Q Corazones]
-- ---------------------------------------------------------------------

-- 1ª solución
cartasBarajadas :: Int -> IO [Carta]
cartasBarajadas n = do
  cs <- permutacion cartas
  return (take n cs)

-- 2ª solución
cartasBarajadas2 :: Int -> IO [Carta]
cartasBarajadas2 n =
  take n <$> permutacion cartas
