-- Estadistica_descriptiva_con_librerias.hs
-- Estadística descriptiva con librerías.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Estadistica_descriptiva_con_librerias where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es redefinir algunas medidas
-- estadísticas de centralización vista en la relación anterior usando
-- las librerías de estadística
--    Statistics.Sample           http://bit.ly/2VI7Rn5
--    Statistics.LinearRegression http://bit.ly/2VM3gAp

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Data.Vector (fromList)
import Statistics.Sample
import Statistics.LinearRegression

-- ---------------------------------------------------------------------
-- Medidas de centralización                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    media :: [Double] -> Double
-- tal que (media xs) es la media aritmética de los números de la lista
-- xs. Por ejemplo,
--    media [4,8,4,5,9]  ==  6.0
-- ---------------------------------------------------------------------

media :: [Double] -> Double
media = mean . fromList

-- ---------------------------------------------------------------------
-- Ejercicio 6. La media geométrica de una lista de n números es la
-- raíz n-ésima del producto de todos los números.
--
-- Definir la función
--    mediaGeometrica :: [Double] -> Double
-- tal que (mediaGeometrica xs) es la media geométrica de xs. Por
-- ejemplo,
--    mediaGeometrica [2,18]   ==  6.0
--    mediaGeometrica [3,1,9]  ==  3.0000000000000004
-- ---------------------------------------------------------------------

mediaGeometrica :: [Double] -> Double
mediaGeometrica = geometricMean . fromList

-- ---------------------------------------------------------------------
-- Medidas de dispersión                                              --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 8. El recorrido (o rango) de una lista de valores es la
-- diferencia entre el mayor y el menor.
--
-- Definir la función
--    [Double] -> Double
-- tal que (rango xs) es el rango de xs. Por ejemplo,
--    rango [4,2,4,7,3]  ==  5.0
-- ---------------------------------------------------------------------

rango :: [Double] -> Double
rango = range . fromList

-- ---------------------------------------------------------------------
-- Ejercicio 10. La varianza de una lista datos es la media de los
-- cuadrados de las distancias de los datos a la media. Por ejemplo, la
-- varianza de [4,8,4,5,9] es 4.4 ya que la media de [4,8,4,5,9] es 6 y
--      ((4-6)^2 + (8-6)^2 + (4-6)^2 + (5-6)^2 + (9-6)^2) / 5
--    = (4 + 4 + 4 + 1 + 9) / 5
--    = 4.4
--
-- Definir la función
--    varianza :: [Double] -> Double
-- tal que (desviacionMedia xs) es la varianza de xs. Por ejemplo,
--    varianza [4,8,4,5,9]       ==  4.4
--    varianza (replicate 10 3)  ==  0.0
-- ---------------------------------------------------------------------

varianza :: [Double] -> Double
varianza = fastVariance . fromList

-- ---------------------------------------------------------------------
-- Ejercicio 11. La desviación típica de una lista de datos es la raíz
-- cuadrada de su varianza.
--
-- Definir la función
--    desviacionTipica :: [Double] -> Double
-- tal que (desviacionTipica xs) es la desviación típica de xs. Por
-- ejemplo,
--    desviacionTipica [4,8,4,5,9]       ==  2.0976176963403033
--    desviacionTipica (replicate 10 3)  ==  0.0
-- ---------------------------------------------------------------------

desviacionTipica :: [Double] -> Double
desviacionTipica = fastStdDev . fromList

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
regresionLineal xs ys =
  linearRegression (fromList xs) (fromList ys)
