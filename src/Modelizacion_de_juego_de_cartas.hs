-- Modelizacion_de_juego_de_cartas.hs
-- Tipos de datos: Modelización de juego de cartas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Modelizacion_de_juego_de_cartas where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se estudia la modelización de un juego de cartas
-- como aplicación de los tipos de datos algebraicos. Además, se definen
-- los generadores correspondientes para comprobar las propiedades con
-- QuickCheck.
--
-- Estos ejercicios corresponden al tema 9 cuyas transparencias se
-- encuentran en
--    https://jaalonso.github.io/cursos/i1m/temas/tema-9.html

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio resuelto. Definir el tipo de datos Palo para representar
-- los cuatro palos de la baraja: picas, corazones, diamantes y
-- tréboles. Hacer que Palo sea instancia de Eq y Show.
-- ---------------------------------------------------------------------

-- La definición es
data Palo = Picas | Corazones | Diamantes | Treboles
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Nota: Para que QuickCheck pueda generar elementos del tipo Palo se
-- usa la siguiente función.
-- ---------------------------------------------------------------------

instance Arbitrary Palo where
  arbitrary = elements [Picas, Corazones, Diamantes, Treboles]

-- ---------------------------------------------------------------------
-- Ejercicio resuelto. Definir el tipo de dato Color para representar los
-- colores de las cartas: rojo y negro. Hacer que Color sea instancia de
-- Eq y de Show.
-- ---------------------------------------------------------------------

data Color = Rojo | Negro
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    color :: Palo -> Color
-- tal que (color p) es el color del palo p. Por ejemplo,
--    color Corazones ==  Rojo
-- Nota: Los corazones y los diamantes son rojos. Las picas y los
-- tréboles son negros.
-- ---------------------------------------------------------------------

color :: Palo -> Color
color Picas     = Negro
color Corazones = Rojo
color Diamantes = Rojo
color Treboles  = Negro

-- ---------------------------------------------------------------------
-- Ejercicio resuelto. Los valores de las cartas se dividen en los
-- numéricos (del 2 al 10) y las figuras (sota, reina, rey y
-- as). Definir el tipo de datos Valor para representar los valores
-- de las cartas. Hacer que Valor sea instancia de Eq y Show.
--    λ> :type Sota
--    Sota :: Valor
--    λ> :type Reina
--    Reina :: Valor
--    λ> :type Rey
--    Rey :: Valor
--    λ> :type As
--    As :: Valor
--    λ> :type Numerico 3
--    Numerico 3 :: Valor
-- ---------------------------------------------------------------------

data Valor = Numerico Int | Sota | Reina | Rey | As
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Nota: Para que QuickCheck pueda generar elementos del tipo Valor se
-- usa la siguiente función.
-- ---------------------------------------------------------------------

instance Arbitrary Valor where
  arbitrary =
    oneof $
      [ do return c
      | c <- [Sota,Reina,Rey,As]
      ] ++
      [ do n <- choose (2,10)
           return (Numerico n)
      ]

-- ---------------------------------------------------------------------
-- Ejercicio 2. El orden de valor de las cartas (de mayor a menor) es
-- as, rey, reina, sota y las numéricas según su valor. Definir la
-- función
--    mayor :: Valor -> Valor -> Bool
-- tal que (mayor x y) se verifica si la carta x es de mayor valor que
-- la carta y. Por ejemplo,
--    mayor Sota (Numerico 7)    ==  True
--    mayor (Numerico 10) Reina  ==  False
-- ---------------------------------------------------------------------

mayor :: Valor -> Valor -> Bool
mayor _            As           = False
mayor As           _            = True
mayor _            Rey          = False
mayor Rey          _            = True
mayor _            Reina        = False
mayor Reina        _            = True
mayor _            Sota         = False
mayor Sota         _            = True
mayor (Numerico m) (Numerico n) = m > n

-- ---------------------------------------------------------------------
-- Ejercicio 3. Comprobar con QuickCheck si dadas dos cartas, una
-- siempre tiene mayor valor que la otra. En caso de que no se verifique,
-- añadir la menor precondición para que lo haga.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_MayorValor1 :: Valor -> Valor -> Bool
prop_MayorValor1 a b =
  mayor a b || mayor b a

-- La comprobación es
--    λ> quickCheck prop_MayorValor1
--    Falsifiable, after 2 tests:
--    Sota
--    Sota
-- que indica que la propiedad es falsa porque la sota no tiene mayor
-- valor que la sota.

-- La precondición es que las cartas sean distintas:
prop_MayorValor :: Valor -> Valor -> Property
prop_MayorValor a b =
  a /= b ==> mayor a b || mayor b a

-- La comprobación es
--    λ> quickCheck prop_MayorValor
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio resuelto. Definir el tipo de datos Carta para representar
-- las cartas mediante un valor y un palo. Hacer que Carta sea instancia
-- de Eq y Show. Por ejemplo,
--    λ> :type Carta Rey Corazones
--    Carta Rey Corazones :: Carta
--    λ> :type Carta (Numerico 4) Corazones
--    Carta (Numerico 4) Corazones :: Carta
-- ---------------------------------------------------------------------

data Carta = Carta Valor Palo
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    valor :: Carta -> Valor
-- tal que (valor c) es el valor de la carta c. Por ejemplo,
--    valor (Carta Rey Corazones)  ==  Rey
-- ---------------------------------------------------------------------

valor :: Carta -> Valor
valor (Carta v _) = v

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    palo :: Carta -> Valor
-- tal que (palo c) es el palo de la carta c. Por ejemplo,
--    palo (Carta Rey Corazones)  ==  Corazones
-- ---------------------------------------------------------------------

palo :: Carta -> Palo
palo (Carta _ p) = p

-- ---------------------------------------------------------------------
-- Nota: Para que QuickCheck pueda generar elementos del tipo Carta se
-- usa la siguiente función.
-- ---------------------------------------------------------------------

instance Arbitrary Carta where
  arbitrary = do
    v <- arbitrary
    p <- arbitrary
    return (Carta v p)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    ganaCarta :: Palo -> Carta -> Carta -> Bool
-- tal que (ganaCarta p c1 c2) se verifica si la carta c1 le gana a la
-- carta c2 cuando el palo de triunfo es p (es decir, las cartas son del
-- mismo palo y el valor de c1 es mayor que el de c2 o c1 es del palo de
-- triunfo). Por ejemplo,
--    ganaCarta Corazones (Carta Sota Picas) (Carta (Numerico 5) Picas)
--    == True
--    ganaCarta Corazones (Carta (Numerico 3) Picas) (Carta Sota Picas)
--    == False
--    ganaCarta Corazones (Carta (Numerico 3) Corazones) (Carta Sota Picas)
--    == True
--    ganaCarta Treboles (Carta (Numerico 3) Corazones) (Carta Sota Picas)
--    == False
-- ---------------------------------------------------------------------

ganaCarta :: Palo -> Carta -> Carta -> Bool
ganaCarta triunfo c c'
  | palo c == palo c' = mayor (valor c) (valor c')
  | palo c == triunfo = True
  | otherwise         = False

-- ---------------------------------------------------------------------
-- Ejercicio 7. Comprobar con QuickCheck si dadas dos cartas, una
-- siempre gana a la otra.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_GanaCarta :: Palo -> Carta -> Carta -> Bool
prop_GanaCarta t c1 c2 =
  ganaCarta t c1 c2 || ganaCarta t c2 c1

-- La comprobación es
--    λ> quickCheck prop_GanaCarta
--    Falsifiable, after 0 tests:
--    Diamantes
--    Carta Rey Corazones
--    Carta As Treboles
-- que indica que la propiedad no se verifica ya que cuando el triunfo
-- es diamantes, ni el rey de corazones le gana al as de tréboles ni el
-- as de tréboles le gana al rey de corazones.

-- ---------------------------------------------------------------------
-- Ejercicio resuelto. Definir el tipo de datos Mano para representar
-- una  mano en el juego de cartas. Una mano es vacía o se obtiene
-- agregando una carta a una mano. Hacer Mano instancia de Eq y
-- Show. Por ejemplo,
--    λ> :type Agrega (Carta Rey Corazones) Vacia
--    Agrega (Carta Rey Corazones) Vacia :: Mano
-- ---------------------------------------------------------------------

data Mano = Vacia | Agrega Carta Mano
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Nota: Para que QuickCheck pueda generar elementos del tipo Mano se
-- usa la siguiente función.
-- ---------------------------------------------------------------------

instance Arbitrary Mano where
  arbitrary = do
    cs <- arbitrary
    let mano []     = Vacia
        mano (c:ds) = Agrega c (mano ds)
    return (mano cs)

-- ---------------------------------------------------------------------
-- Ejercicio 8. Una mano gana a una carta c si alguna carta de la mano
-- le gana a c. Definir la función
--    ganaMano :: Palo -> Mano -> Carta -> Bool
-- tal que (gana t m c) se verifica si la mano m le gana a la carta c
-- cuando el triunfo es t. Por ejemplo,
--    ganaMano Picas (Agrega (Carta Sota Picas) Vacia) (Carta Rey Corazones)
--    ==  True
--    ganaMano Picas (Agrega (Carta Sota Picas) Vacia) (Carta Rey Picas)
--    ==  False
-- ---------------------------------------------------------------------

ganaMano :: Palo -> Mano -> Carta -> Bool
ganaMano _       Vacia        _  = False
ganaMano triunfo (Agrega c m) c' = ganaCarta triunfo c c' ||
                                   ganaMano triunfo m c'

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    eligeCarta :: Palo -> Carta -> Mano -> Carta
-- tal que (eligeCarta t c1 m) es la mejor carta de la mano m frente a
-- la carta c cuando el triunfo es t. La estrategia para elegir la mejor
-- carta es la siguiente:
-- + Si la mano sólo tiene una carta, se elige dicha carta.
-- + Si la primera carta de la mano es del palo de c1 y la mejor del
--   resto no es del palo de c1, se elige la primera de la mano,
-- + Si la primera carta de la mano no es del palo de c1 y la mejor
--   del resto es del palo de c1, se elige la mejor del resto.
-- + Si la primera carta de la mano le gana a c1 y la mejor del
--   resto no le gana a c1, se elige la primera de la mano,
-- + Si la mejor del resto le gana a c1 y la primera carta de la mano
--   no le gana a c1, se elige la mejor del resto.
-- + Si el valor de la primera carta es mayor que el de la mejor del
--   resto, se elige la mejor del resto.
-- + Si el valor de la primera carta no es mayor que el de la mejor
--   del resto, se elige la primera carta.
-- ---------------------------------------------------------------------

eligeCarta :: Palo -> Carta -> Mano -> Carta
eligeCarta _       _  (Agrega c Vacia) = c                        -- 1
eligeCarta triunfo c1 (Agrega c resto)
  | palo c == palo c1 && palo c' /= palo c1                  = c  -- 2
  | palo c /= palo c1 && palo c' == palo c1                  = c' -- 3
  | ganaCarta triunfo c  c1 && not (ganaCarta triunfo c' c1) = c  -- 4
  | ganaCarta triunfo c' c1 && not (ganaCarta triunfo c c1)  = c' -- 5
  | mayor (valor c) (valor c')                               = c' -- 6
  | otherwise                                                = c  -- 7
 where
  c' = eligeCarta triunfo c1 resto
eligeCarta _ _ _ = error "Imposible"

-- ---------------------------------------------------------------------
-- Ejercicio 10. Comprobar con QuickCheck que si una mano es ganadora,
-- entonces la carta elegida es ganadora.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_eligeCartaGanaSiEsPosible :: Palo -> Carta -> Mano -> Property
prop_eligeCartaGanaSiEsPosible triunfo c m =
  m /= Vacia ==>
  ganaMano triunfo m c == ganaCarta triunfo (eligeCarta triunfo c m) c

-- La comprobación es
--    λ> quickCheck prop_eligeCartaGanaSiEsPosible
--    Falsifiable, after 12 tests:
--    Corazones
--    Carta Rey Treboles
--    Agrega (Carta (Numerico 6) Diamantes)
--           (Agrega (Carta Sota Picas)
--            (Agrega (Carta Rey Corazones)
--             (Agrega (Carta (Numerico 10) Treboles)
--              Vacia)))
-- La carta elegida es el 10 de tréboles (porque tiene que ser del mismo
-- palo), aunque el mano hay una carta (el rey de corazones) que gana.
