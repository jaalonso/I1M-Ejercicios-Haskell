-- Calculo_numerico_2_Limites_biseccion_e_integrales.hs
-- Cálculo numérico (2): Límites, bisección e integrales.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Calculo_numerico_2_Limites_biseccion_e_integrales where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se definen funciones para resolver los siguientes
-- problemas de cálculo numérico:
-- + Cálculo de límites.
-- + Cálculo de los ceros de una función por el método de la bisección.
-- + Cálculo de raíces enteras.
-- + Cálculo de integrales por el método de los rectángulos.
-- + Algoritmo de bajada para resolver un sistema triangular inferior.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.Matrix

-- ---------------------------------------------------------------------
-- § Cálculo de límites                                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    limite :: (Double -> Double) -> Double -> Double
-- tal que (limite f a) es el valor de f en el primer término x tal que,
-- para todo y entre x+1 y x+100, el valor absoluto de la diferencia
-- entre f(y) y f(x) es menor que a. Por ejemplo,
--    limite (\n -> (2*n+1)/(n+5)) 0.001  ==  1.9900110987791344
--    limite (\n -> (1+1/n)**n) 0.001     ==  2.714072874546881
-- ---------------------------------------------------------------------

limite :: (Double -> Double) -> Double -> Double
limite f a =
  head [f x | x <- [1..],
              maximum [abs (f y - f x) | y <- [x+1..x+100]] < a]

-- ---------------------------------------------------------------------
-- § Ceros de una función por el método de la bisección               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2. El método de bisección para calcular un cero de una
-- función en el intervalo [a,b] se basa en el teorema de Bolzano:
--    "Si f(x) es una función continua en el intervalo [a, b], y si,
--    además, en los extremos del intervalo la función f(x) toma valores
--    de signo opuesto (f(a) * f(b) < 0), entonces existe al menos un
--    valor c en (a, b) para el que f(c) = 0".
--
-- El método para calcular un cero de la función f en el intervalo [a,b]
-- con un error menor que e consiste en tomar el punto medio del
-- intervalo c = (a+b)/2 y considerar los siguientes casos:
-- (*) Si |f(c)| < e, hemos encontrado una aproximación del punto que
--     anula f en el intervalo con un error aceptable.
-- (*) Si f(c) tiene signo distinto de f(a), repetir el proceso en el
--     intervalo [a,c].
-- (*) Si no, repetir el proceso en el intervalo [c,b].
--
-- Definir la función
--    biseccion :: (Double -> Double) -> Double -> Double -> Double -> Double
-- tal que (biseccion f a b e) es una aproximación del punto del
-- intervalo [a,b] en el que se anula la función f, con un error menor
-- que e, calculada mediante el método de la bisección. Por ejemplo,
--    biseccion (\x -> x^2 - 3) 0 5 0.01             ==  1.7333984375
--    biseccion (\x -> x^3 - x - 2) 0 4 0.01         ==  1.521484375
--    biseccion cos 0 2 0.01                         ==  1.5625
--    biseccion (\x -> log (50-x) - 4) (-10) 3 0.01  ==  -5.125
-- ---------------------------------------------------------------------

-- 1ª solución
biseccion :: (Double -> Double) -> Double -> Double -> Double -> Double
biseccion f a b e
  | abs (f c) < e   = c
  | (f a)*(f c) < 0 = biseccion f a c e
  | otherwise       = biseccion f c b e
  where c = (a+b)/2

-- 2ª solución
biseccion2 :: (Double -> Double) -> Double -> Double -> Double -> Double
biseccion2 f a b e = aux a b
  where aux a' b' | abs (f c) < e   = c
                  | f a' * f c < 0  = aux a' c
                  | otherwise       = aux c b'
          where c = (a'+b')/2

-- ---------------------------------------------------------------------
-- § Cálculo de raíces enteras                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    raizEnt :: Integer -> Integer -> Integer
-- tal que (raizEnt x n) es la raíz entera n-ésima de x; es decir, el
-- mayor número entero y tal que y^n <= x. Por ejemplo,
--    raizEnt  8 3      ==  2
--    raizEnt  9 3      ==  2
--    raizEnt 26 3      ==  2
--    raizEnt 27 3      ==  3
--    raizEnt (10^50) 2 ==  10000000000000000000000000
--
-- Comprobar con QuickCheck que para todo número natural n,
--     raizEnt (10^(2*n)) 2 == 10^n
-- ---------------------------------------------------------------------

-- 1ª definición
raizEnt1 :: Integer -> Integer -> Integer
raizEnt1 x n =
  last (takeWhile (\y -> y^n <= x) [0..])

-- 2ª definición
raizEnt2 :: Integer -> Integer -> Integer
raizEnt2 x n =
  floor ((fromIntegral x)**(1 / fromIntegral n))

-- Nota. La definición anterior falla para números grandes. Por ejemplo,
--    λ> raizEnt2 (10^50) 2 == 10^25
--    False

-- 3ª definición
raizEnt3 :: Integer -> Integer -> Integer
raizEnt3 x n = aux (1,x)
  where aux (a,b) | d == x    = c
                  | c == a    = c
                  | d < x     = aux (c,b)
                  | otherwise = aux (a,c)
          where c = (a+b) `div` 2
                d = c^n

-- Comparación de eficiencia
--    λ> raizEnt1 (10^14) 2
--    10000000
--    (6.15 secs, 6,539,367,976 bytes)
--    λ> raizEnt2 (10^14) 2
--    10000000
--    (0.00 secs, 0 bytes)
--    λ> raizEnt3 (10^14) 2
--    10000000
--    (0.00 secs, 25,871,944 bytes)
--
--    λ> raizEnt2 (10^50) 2
--    9999999999999998758486016
--    (0.00 secs, 0 bytes)
--    λ> raizEnt3 (10^50) 2
--    10000000000000000000000000
--    (0.00 secs, 0 bytes)

-- La propiedad es
prop_raizEnt :: Integer -> Bool
prop_raizEnt n =
  raizEnt3 (10^(2*m)) 2 == 10^m
  where m = abs n

-- La comprobación es
--    λ> quickCheck prop_raizEnt
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Integración por el método de los rectángulos                     --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4. La integral definida de una función f entre los límites
-- a y b puede calcularse mediante la regla del rectángulo
-- (ver en http://bit.ly/1FDhZ1z) usando la fórmula
--    h * (f(a+h/2) + f(a+h+h/2) + f(a+2h+h/2) + ... + f(a+n*h+h/2))
-- con a+n*h+h/2 <= b < a+(n+1)*h+h/2 y usando valores pequeños para h.
--
-- Definir la función
--    integral :: (Fractional a, Ord a) => a -> a -> (a -> a) -> a -> a
-- tal que (integral a b f h) es el valor de dicha expresión. Por
-- ejemplo, el cálculo de la integral de f(x) = x^3 entre 0 y 1, con
-- paso 0.01, es
--    integral 0 1 (^3) 0.01  ==  0.24998750000000042
-- Otros ejemplos son
--    integral 0 1 (^4) 0.01                        ==  0.19998333362500048
--    integral 0 1 (\x -> 3*x^2 + 4*x^3) 0.01       ==  1.9999250000000026
--    log 2 - integral 1 2 (\x -> 1/x) 0.01         ==  3.124931644782336e-6
--    pi - 4 * integral 0 1 (\x -> 1/(x^2+1)) 0.01  ==  -8.333333331389525e-6
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

integral :: (Fractional a, Ord a) => a -> a -> (a -> a) -> a -> a
integral a b f h = h * suma (a+h/2) b (+h) f

-- (suma a b s f) es l valor de
--    f(a) + f(s(a)) + f(s(s(a)) + ... + f(s(...(s(a))...))
-- hasta que s(s(...(s(a))...)) > b. Por ejemplo,
--    suma 2 5 (1+) (^3)  ==  224
suma :: (Ord t, Num a) => t -> t -> (t -> t) -> (t -> a) -> a
suma a b s f = sum [f x | x <- sucesion a b s]

-- (sucesion x y s) es la lista
--    [a, s(a), s(s(a), ..., s(...(s(a))...)]
-- hasta que s(s(...(s(a))...)) > b. Por ejemplo,
--    sucesion 3 20 (+2)  ==  [3,5,7,9,11,13,15,17,19]
sucesion :: Ord a => a -> a -> (a -> a) -> [a]
sucesion a b s = takeWhile (<=b) (iterate s a)

-- 2ª solución
-- ===========

integral2 :: (Fractional a, Ord a) => a -> a -> (a -> a) -> a -> a
integral2 a b f h
  | a+h/2 > b = 0
  | otherwise = h * f (a+h/2) + integral2 (a+h) b f h

-- 3ª solución
-- ===========

integral3 :: (Fractional a, Ord a) => a -> a -> (a -> a) -> a -> a
integral3 a b f h = aux a where
  aux x | x+h/2 > b = 0
        | otherwise = h * f (x+h/2) + aux (x+h)

-- Comparación de eficiencia
--    λ> integral 0 10 (^3) 0.00001
--    2499.9999998811422
--    (4.62 secs, 1084774336 bytes)
--    λ> integral2 0 10 (^3) 0.00001
--    2499.999999881125
--    (7.90 secs, 1833360768 bytes)
--    λ> integral3 0 10 (^3) 0.00001
--    2499.999999881125
--    (7.27 secs, 1686056080 bytes)

-- ---------------------------------------------------------------------
-- § Algoritmo de bajada para resolver un sistema triangular inferior --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5. Un sistema de ecuaciones lineales Ax = b es triangular
-- inferior si todos los elementos de la matriz A que están por encima
-- de la diagonal principal son nulos; es decir, es de la forma
--    a(1,1)*x(1)                                               = b(1)
--    a(2,1)*x(1) + a(2,2)*x(2)                                 = b(2)
--    a(3,1)*x(1) + a(3,2)*x(2) + a(3,3)*x(3)                   = b(3)
--    ...
--    a(n,1)*x(1) + a(n,2)*x(2) + a(n,3)*x(3) +...+ a(x,x)*x(n) = b(n)
--
-- El sistema es compatible si, y sólo si, el producto de los elementos
-- de la diagonal principal es distinto de cero. En este caso, la
-- solución se puede calcular mediante el algoritmo de bajada:
--    x(1) = b(1) / a(1,1)
--    x(2) = (b(2) - a(2,1)*x(1)) / a(2,2)
--    x(3) = (b(3) - a(3,1)*x(1) - a(3,2)*x(2)) / a(3,3)
--    ...
--    x(n) = (b(n) - a(n,1)*x(1) - a(n,2)*x(2) -...- a(n,n-1)*x(n-1)) / a(n,n)
--
-- Definir la función
--    bajada :: Matrix Double -> Matrix Double -> Matrix Double
-- tal que (bajada a b) es la solución, mediante el algoritmo de bajada,
-- del sistema compatible triangular superior ax = b. Por ejemplo,
--    λ> let a = fromLists [[2,0,0],[3,1,0],[4,2,5.0]]
--    λ> let b = fromLists [[3],[6.5],[10]]
--    λ> bajada a b
--    ( 1.5 )
--    ( 2.0 )
--    ( 0.0 )
-- Es decir, la solución del sistema
--    2x            = 3
--    3x + y        = 6.5
--    4x + 2y + 5 z = 10
-- es x=1.5, y=2 y z=0.
-- ---------------------------------------------------------------------

bajada :: Matrix Double -> Matrix Double -> Matrix Double
bajada a b = fromLists [[x i] | i <- [1..m]]
  where m   = nrows a
        x k = (b!(k,1) - sum [a!(k,j) * x j | j <- [1..k-1]]) / a!(k,k)
