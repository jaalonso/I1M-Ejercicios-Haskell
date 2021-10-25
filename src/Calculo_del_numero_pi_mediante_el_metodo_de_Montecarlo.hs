-- Calculo_del_numero_pi_mediante_el_metodo_de_Montecarlo.hs
-- Cálculo del número pi mediante el método de Montecarlo.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Calculo_del_numero_pi_mediante_el_metodo_de_Montecarlo where

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es el uso de los números
-- aleatorios para calcular el número pi mediante el método de
-- Montecarlo. Un ejemplo del método se puede leer en el artículo de
-- Pablo Rodríguez "Calculando pi con gotas de lluvia" que se encuentra
-- en http://bit.ly/1cNfSR0

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import System.Random
import Graphics.Gnuplot.Simple

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    puntosDelCuadrado :: Int -> IO [(Double,Double)]
-- tal que (puntosDelCuadrado n) es una lista aleatoria de n puntos del
-- cuadrado de vértices opuestos (-1,-1) y (1,1). Por ejemplo,
--    λ> puntosDelCuadrado 2
--    [(0.7071212580055017,0.5333728820632873),
--     (-0.18430740317151528,-0.9996319726105287)]
--    λ> puntosDelCuadrado 2
--    [(-0.45032646341358595,0.30614607738929633),
--     (0.4402992058238284,0.5810531167431172)]
-- ---------------------------------------------------------------------

puntosDelCuadrado :: Int -> IO [(Double,Double)]
puntosDelCuadrado n = do
  gen <- newStdGen
  let xs = randomRs (-1,1) gen
      (as, ys) = splitAt n xs
      (bs, _)  = splitAt n ys
  return (zip as bs)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    puntosEnElCirculo :: [(Double,Double)] -> Int
-- tal que (puntosEnElCirculo xs) es el número de puntos de la lista xs
-- que están en el círculo de centro (0,0) y radio 1.
--    puntosEnElCirculo [(1,0), (0.5,0.9), (0.2,-0.3)]  ==  2
-- ---------------------------------------------------------------------

puntosEnElCirculo :: [(Double,Double)] -> Int
puntosEnElCirculo xs =
  length [(x,y) | (x,y) <- xs
                , x^2+y^2 <= 1]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    calculoDePi :: Int -> Double
-- tal que (calculoDePi n) es el cálculo del número pi usando n puntos
-- aleatorios (la probabilidad de que estén en el círculo es pi/4). Por
-- ejemplo,
--    λ> calculoDePi 1000
--    3.088
--    λ> calculoDePi 1000
--    3.184
--    λ> calculoDePi 10000
--    3.1356
--    λ> calculoDePi 100000
--    3.13348
-- ---------------------------------------------------------------------

calculoDePi :: Int -> IO Double
calculoDePi n = do
  xs <- puntosDelCuadrado n
  let enCirculo = fromIntegral (puntosEnElCirculo xs)
      total     = fromIntegral n
  return (4 * enCirculo / total)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    graficaPi :: [Int] -> IO ()
-- tal que (graficaPi xs) dibuja la grafica del valor de pi usando el
-- número de putos indicados por los elementos de xs. Por ejemplo,
-- (graficaPi [0,10..4000]) dibuja la Figura 1 (ver
-- https://bit.ly/3pzAO6N  ).
-- -----------------------------------------------------------------------

graficaPi :: [Int] -> IO ()
graficaPi xs = do
  ys <- mapM calculoDePi xs
  plotLists [Key Nothing]
            [ zip xs ys
            , zip xs (repeat pi) ]
