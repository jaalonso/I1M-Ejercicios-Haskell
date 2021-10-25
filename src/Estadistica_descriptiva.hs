-- Estadistica_descriptiva.hs
-- Estadística descriptiva.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Estadistica_descriptiva where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es definir las principales medidas
-- estadísticas de centralización (medias, mediana y modas) y de
-- dispersión (rango, desviación media, varianza y desviación típica)
-- que se estudian en 3º de ESO (como en http://bit.ly/2FbXOQm ).
--
-- En las soluciones de los ejercicios se pueden usar las siguientes
-- funciones de la librería Data.List: fromIntegral, genericLength, sort,
-- y group (cuya descripción se puede consultar en el "Manual de
-- funciones básicas de Haskell" http://bit.ly/2VICngx ).

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck
import Graphics.Gnuplot.Simple

-- ---------------------------------------------------------------------
-- Medidas de centralización                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    media :: Floating a => [a] -> a
-- tal que (media xs) es la media aritmética de los números de la lista
-- xs. Por ejemplo,
--    media [4,8,4,5,9]  ==  6.0
-- ---------------------------------------------------------------------

media :: Floating a => [a] -> a
media xs = sum xs / genericLength xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. La mediana de una lista de valores es el valor de
-- la lista que ocupa el lugar central de los valores ordenados de menor
-- a mayor. Si el número de datos es impar se toma como valor de la
-- mediana el valor central. Si el número de datos es par se toma como
-- valor de la mediana la media aritmética de los dos valores
-- centrales.
--
-- Definir la función
--    mediana :: (Floating a, Ord a) => [a] -> a
-- tal que (mediana xs) es la mediana de la lista xs. Por ejemplo,
--    mediana [2,3,6,8,9]    ==  6.0
--    mediana [2,3,4,6,8,9]  ==  5.0
--    mediana [9,6,8,4,3,2]  ==  5.0
-- ---------------------------------------------------------------------

mediana :: (Floating a, Ord a) => [a] -> a
mediana xs | odd n     = ys !! i
           | otherwise = media [ys !! (i-1), ys !! i]
  where ys = sort xs
        n  = length xs
        i  = n `div` 2

-- ---------------------------------------------------------------------
-- Ejercicio 3. Comprobar con QuickCheck que para cualquier lista no
-- vacía xs el número de elementos de xs menores que su mediana es menor
-- o igual que la mitad de los elementos de xs y lo mismo pasa con los
-- mayores o iguales que la mediana.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mediana :: [Double] -> Property
prop_mediana xs =
  not (null xs) ==>
  genericLength [x | x <- xs, x < m] <= n/2 &&
  genericLength [x | x <- xs, x > m] <= n/2
  where m = mediana xs
        n = genericLength xs

-- La comprobación es
--    λ> quickCheck prop_mediana
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    frecuencias :: Ord a => [a] -> [(a,Int)]
-- tal que (frecuencias xs) es la lista formada por los elementos de xs
-- junto con el número de veces que aparecen en xs. Por ejemplo,
--    frecuencias "sosos" ==  [('o',2),('s',3)]
--
-- Nota: El orden de los pares no importa
-- ---------------------------------------------------------------------

-- 1ª solución
frecuencias :: Ord a => [a] -> [(a,Int)]
frecuencias xs = [(y,ocurrencias y xs) | y <- sort (nub xs)]
  where ocurrencias y zs = length [1 | x <- zs, x == y]

-- 2ª solución
frecuencias2 :: Ord a => [a] -> [(a,Int)]
frecuencias2 xs = [(y,1 + length ys) | (y:ys) <- group (sort xs)]

-- 3ª solución
frecuencias3 :: Ord a => [a] -> [(a,Int)]
frecuencias3 = map (\ys@(y:_) -> (y,length ys)) . group . sort

-- ---------------------------------------------------------------------
-- Ejercicio 5. Las modas de una lista son los elementos de la lista
-- que más se repiten.
--
-- Definir la función
--    modas :: Ord a => [a] -> [a]
-- tal que (modas xs) es la lista ordenada de las modas de xs. Por
-- ejemplo
--    modas [7,3,7,5,3,1,6,9,6]  ==  [3,6,7]
-- ---------------------------------------------------------------------

modas :: Ord a => [a] -> [a]
modas xs = [y | (y,n) <- ys, n == m]
  where ys = frecuencias xs
        m  = maximum (map snd ys)

-- ---------------------------------------------------------------------
-- Ejercicio 6. La media geométrica de una lista de n números es la
-- raíz n-ésima del producto de todos los números.
--
-- Definir la función
--    mediaGeometrica :: Floating a => [a] -> a
-- tal que (mediaGeometrica xs) es la media geométrica de xs. Por
-- ejemplo,
--    mediaGeometrica [2,18]   ==  6.0
--    mediaGeometrica [3,1,9]  ==  3.0
-- ---------------------------------------------------------------------

mediaGeometrica :: Floating a => [a] -> a
mediaGeometrica xs = product xs ** (1 / genericLength xs)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Comprobar con QuickCheck que la media geométrica de
-- cualquier lista no vacía de números no negativos es siempre menor o
-- igual que la media aritmética.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mediaGeometrica :: [Double] -> Property
prop_mediaGeometrica xs =
  not (null xs) ==>
  mediaGeometrica ys <= media ys
  where ys = map abs xs

-- La comprobación es
--    λ> quickCheck prop_mediaGeometrica
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Medidas de dispersión                                              --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 8. El recorrido (o rango) de una lista de valores es la
-- diferencia entre el mayor y el menor.
--
-- Definir la función
--    rango :: (Num a, Ord a) => [a] -> a
-- tal que (rango xs) es el rango de xs. Por ejemplo,
--    rango [4,2,4,7,3]  ==  5
-- ---------------------------------------------------------------------

-- 1ª solución
rango :: (Num a, Ord a) => [a] -> a
rango xs = maximum xs - minimum xs

-- 2ª solución
rango2 :: (Num a, Ord a) => [a] -> a
rango2 xs = maximo - minimo
  where (y:ys)           = xs
        (minimo,maximo)  = aux ys (y,y)
        aux [] (a,b)     = (a,b)
        aux (z:zs) (a,b) = aux zs (min a z, max z b)

-- ---------------------------------------------------------------------
-- Ejercicio 9. La desviación media de una lista de datos xs es la
-- media de las distancias de los datos a la media xs, donde la
-- distancia entre dos elementos es el valor absoluto de su
-- diferencia. Por ejemplo, la desviación media de [4,8,4,5,9] es 2 ya
-- que la media de [4,8,4,5,9] es 6 y
--      (|4-6| + |8-6| + |4-6| + |5-6| + |9-6|) / 5
--    = (2 + 2 + 2 + 1 + 3) / 5
--    = 2
--
-- Definir la función
--    desviacionMedia :: Floating a => [a] -> a
-- tal que (desviacionMedia xs) es la desviación media de xs. Por
-- ejemplo,
--    desviacionMedia [4,8,4,5,9]       ==  2.0
--    desviacionMedia (replicate 10 3)  ==  0.0
-- ---------------------------------------------------------------------

desviacionMedia :: Floating a => [a] -> a
desviacionMedia xs = media [abs (x - m) | x <- xs]
  where m = media xs

-- ---------------------------------------------------------------------
-- Ejercicio 10. La varianza de una lista datos es la media de los
-- cuadrados de las distancias de los datos a la media. Por ejemplo, la
-- varianza de [4,8,4,5,9] es 4.4 ya que la media de [4,8,4,5,9] es 6 y
--      ((4-6)^2 + (8-6)^2 + (4-6)^2 + (5-6)^2 + (9-6)^2) / 5
--    = (4 + 4 + 4 + 1 + 9) / 5
--    = 4.4
--
-- Definir la función
--    varianza :: Floating a => [a] -> a
-- tal que (desviacionMedia xs) es la varianza de xs. Por ejemplo,
--    varianza [4,8,4,5,9]       ==  4.4
--    varianza (replicate 10 3)  ==  0.0
-- ---------------------------------------------------------------------

varianza :: Floating a => [a] -> a
varianza xs = media [(x-m)^2 | x <- xs]
  where m = media xs

-- ---------------------------------------------------------------------
-- Ejercicio 11. La desviación típica de una lista de datos es la raíz
-- cuadrada de su varianza.
--
-- Definir la función
--    desviacionTipica :: Floating a => [a] -> a
-- tal que (desviacionTipica xs) es la desviación típica de xs. Por
-- ejemplo,
--    desviacionTipica [4,8,4,5,9]       ==  2.0976176963403033
--    desviacionTipica (replicate 10 3)  ==  0.0
-- ---------------------------------------------------------------------

-- 1ª definición
desviacionTipica :: Floating a => [a] -> a
desviacionTipica xs = sqrt (varianza xs)

-- 2ª definición
desviacionTipica2 :: Floating a => [a] -> a
desviacionTipica2 = sqrt . varianza

-- ---------------------------------------------------------------------
-- Regresión lineal                                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 12. Dadas dos listas de valores
--    xs = [x(1), x(2), ..., x(n)]
--    ys = [y(1), y(2), ..., y(n)]
-- la ecuación de la recta de regresión de ys sobre xs es y = a+bx,
-- donde
--    b = (nΣx(i)y(i) - Σx(i)Σy(i)) / (nΣx(i)² - (Σx(i))²)
--    a = (Σy(i) - bΣx(i)) / n
--
-- Definir la función
--    regresionLineal :: [Double] -> [Double] -> (Double,Double)
-- tal que (regresionLineal xs ys) es el par (a,b) de los coeficientes
-- de la recta de regresión de ys sobre xs. Por ejemplo, para los
-- valores
--    ejX, ejY :: [Double]
--    ejX = [5,  7, 10, 12, 16, 20, 23, 27, 19, 14]
--    ejY = [9, 11, 15, 16, 20, 24, 27, 29, 22, 20]
-- se tiene
--    λ> regresionLineal ejX ejY
--    (5.195045748716805,0.9218924347243919)
-- ---------------------------------------------------------------------

ejX, ejY :: [Double]
ejX = [5,  7, 10, 12, 16, 20, 23, 27, 19, 14]
ejY = [9, 11, 15, 16, 20, 24, 27, 29, 22, 20]

regresionLineal :: [Double] -> [Double] -> (Double,Double)
regresionLineal xs ys = (a,b)
  where n     = genericLength xs
        sumX  = sum xs
        sumY  = sum ys
        sumX2 = sum (zipWith (*) xs xs)
        sumXY = sum (zipWith (*) xs ys)
        b     = (n*sumXY - sumX*sumY) / (n*sumX2 - sumX^2)
        a     = (sumY - b*sumX) / n

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir el procedimiento
--    grafica :: [Double] -> [Double] -> IO ()
-- tal que (grafica xs ys) pinte los puntos correspondientes a las
-- listas de valores xs e ys y su recta de regresión. Por ejemplo,
-- con (grafica ejX ejY) se obtiene el dibujo de la Figura 1
-- que se encuentra en
-- ---------------------------------------------------------------------

grafica :: [Double] -> [Double] -> IO ()
grafica xs ys =
  plotPathsStyle
    [YRange (0,10+mY)]
    [(defaultStyle {plotType = Points,
                    lineSpec = CustomStyle [LineTitle "Datos",
                                            PointType 2,
                                            PointSize 2.5]},
                   zip xs ys),
     (defaultStyle {plotType = Lines,
                    lineSpec = CustomStyle [LineTitle "Ajuste",
                                            LineWidth 2]},
                   [(x,a+b*x) | x <- [0..mX]])]
  where (a,b) = regresionLineal xs ys
        mX    = maximum xs
        mY    = maximum ys
