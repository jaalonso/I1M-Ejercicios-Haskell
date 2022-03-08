-- Transacciones.hs
-- Transacciones.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- En esta relación se continúa el estudio de las cadenas de bloques (en
-- inglés, "blockchain") que empezamos en la relación Cadenas_de_bloques.hs
-- https://bit.ly/3trXIgW
--
-- El objetivo de la relación es el estudio de las transacciones, que
-- forman el contenido de las cadenas de bloques.

module Transacciones where

import Data.Maybe (fromMaybe)
import Prelude    hiding (lookup)
import Data.Map   (Map, alter, findWithDefault, insert, lookup)
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir el tipo Importe para representar el importe de
-- las transacciones como sinónimo de Int.
-- ---------------------------------------------------------------------

type Importe  = Int

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir el tipo Cuenta para representar las cuentas de
-- las transacciones como sinónimo de String.
-- ---------------------------------------------------------------------

type Cuenta = String

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir el tipo de dato Transaccion con tres
-- constructores:
-- + trImporte con el importe de la transacción,
-- + trDe con la cuenta del emisor y
-- + trA con la cuenta del receptor.
-- ---------------------------------------------------------------------

data Transaccion = Transaccion { trImporte :: Importe
                               , trDe      :: Cuenta
                               , trA       :: Cuenta
                               }
  deriving (Eq, Show)

-- En los ejemplos se usarán las siguientes transacciones:
transaccion1, transaccion2 :: Transaccion
transaccion1 = Transaccion 10 "Ana" "Luis"
transaccion2 = Transaccion { trImporte = 7
                           , trDe      = "Luis"
                           , trA       = "Abel"
                           }

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir el procedimiento
--    transaccionArbitraria :: Gen Transaccion
-- tal que transiccionArbitraria es una transicción aleatoria. Por
-- ejemplo,
--    λ> sample transaccionArbitraria
--    Transaccion {trImporte = 0, trDe = "adefklmnopqvw", trA = "adgiklnoqstuw"}
--    Transaccion {trImporte = 0, trDe = "abcdehjmopqruwxz", trA = "abcfgkmoqrtvwx"}
--    Transaccion {trImporte = 0, trDe = "acdegjklprtvw", trA = "abdefghijklnoptvx"}
--    Transaccion {trImporte = 6, trDe = "aefhjklnoqxz", trA = "abfikmosvwx"}
--    Transaccion {trImporte = -1, trDe = "bdfghjklnuvwxy", trA = "begjklmruv"}
--    Transaccion {trImporte = -4, trDe = "abcdiklnprsux", trA = "acdghijklnoprsxz"}
--    Transaccion {trImporte = 3, trDe = "cijoprstux", trA = "bdfmnoqrsuxy"}
--    Transaccion {trImporte = 5, trDe = "cdhijlnqruv", trA = "ceghijklmortwxy"}
--    Transaccion {trImporte = 9, trDe = "cdfhijlrtuwxyz", trA = "abcdegijlptuw"}
--    Transaccion {trImporte = 0, trDe = "cdefgijkmnpstuy", trA = "bcdfpqrtuwyz"}
--    Transaccion {trImporte = -17, trDe = "chklmnqsuvw", trA = "adhilmnopqrwxy"}
-- ---------------------------------------------------------------------

transaccionArbitraria :: Gen Transaccion
transaccionArbitraria = do
  i <- arbitrary
  d <- sublistOf ['a'..'z']
  a <- sublistOf ['a'..'z']
  return (Transaccion i d a)

-- ---------------------------------------------------------------------
-- Ejercicio 5. Declarar Transiccion como subclase de Arbitraria usando
-- el generador transiccionArbitraria.
-- ---------------------------------------------------------------------

instance Arbitrary Transaccion where
  arbitrary = transaccionArbitraria
  shrink (Transaccion a f t) = [Transaccion a' f  t  | a' <- shrink a] ++
                               [Transaccion a  f' t  | f' <- shrink f] ++
                               [Transaccion a  f  t' | t' <- shrink t]

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    inversa :: Transaccion -> Transaccion

-- tal que (inversa t) es la transicción cuyo importe y dirección son
-- opuestos a los de t. Por ejemplo,
--    λ> inversa (Transaccion {trImporte = 10, trDe = "Ana", trA = "Luis"})
--    Transaccion {trImporte = -10, trDe = "Luis", trA = "Ana"}
-- ---------------------------------------------------------------------

inversa :: Transaccion -> Transaccion
inversa t = Transaccion { trImporte = - (trImporte t)
                        , trDe      = trA t
                        , trA       = trDe t
                        }

-- ---------------------------------------------------------------------
-- Ejercicio 7. Comprobar con QuickCheck que la función inversa es
-- involutiva.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_inversa_inversa :: Transaccion -> Property
prop_inversa_inversa t = inversa (inversa t) === t

-- La comprobación es
--    λ> quickCheck prop_inversa_inversa
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    normalizada :: Transaccion -> Transaccion
-- tal que (normalizada t) es la inversa de t, si el importe de t es
-- negativo y es t, en caso contrario. Por ejemplo,
--    λ> normalizada (Transaccion {trImporte = -5, trDe = "Ana", trA = "Luis"})
--    Transaccion {trImporte = 5, trDe = "Luis", trA = "Ana"}
--    λ> normalizada (Transaccion {trImporte = 7, trDe = "Ana", trA = "Luis"})
--    Transaccion {trImporte = 7, trDe = "Ana", trA = "Luis"}
-- ---------------------------------------------------------------------

normalizada :: Transaccion -> Transaccion
normalizada t
  | trImporte t < 0 = inversa t
  | otherwise       = t

-- ---------------------------------------------------------------------
-- Ejercicio 9. Comprobar con QuickCheck que la función normalizada es
-- idempotente.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_normalizada_normalizada :: Transaccion -> Property
prop_normalizada_normalizada t =
  normalizada (normalizada t) === normalizada t

-- La comprobación es
--    λ> quickCheck prop_normalizada_normalizada
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10. Comprobar con QuickCheck que el importe de las
-- transacciones normalizadas son no negativos.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_normalizada_no_negativa :: Transaccion -> Bool
prop_normalizada_no_negativa t =
  trImporte (normalizada t) >= 0

-- La comprobación es
--    λ> quickCheck prop_normalizada_no_negativa
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir el tipo Cuentas como sinónimo de los
-- diccionarios con claves de tipo Cuenta y valores de tipo Importe.
-- ---------------------------------------------------------------------

type Cuentas = Map Cuenta Importe

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    procesaTransaccion :: Transaccion -> Cuentas -> Cuentas
-- tal que (procesaTransaccion t cs) es el estado de las cuentas
-- obtenido después de aplicar la transacción t a cs. Por ejemplo,
--    λ> procesaTransaccion (Transaccion 60 "Ana" "Bea") (fromList [("Ana",90)])
--    fromList [("Ana",30),("Bea",60)]
--    λ> procesaTransaccion (Transaccion 40 "Bea" "Ana") it
--    fromList [("Ana",70),("Bea",20)]
--    λ> procesaTransaccion (Transaccion 40 "Bea" "Eva") it
--    fromList [("Ana",70),("Bea",-20),("Eva",40)]
-- ---------------------------------------------------------------------

-- 1ª definición
procesaTransaccion :: Transaccion -> Cuentas -> Cuentas
procesaTransaccion (Transaccion i d a) cs =
  insert d (sd - i) (insert a (sa + i) cs)
  where sd = fromMaybe 0 (lookup d cs)
        sa = fromMaybe 0 (lookup a cs)

-- 2ª definición
procesaTransaccion2 :: Transaccion -> Cuentas -> Cuentas
procesaTransaccion2 (Transaccion i d a) cs =
  insert d (sd - i) (insert a (sa + i) cs)
  where sd = findWithDefault 0 d cs
        sa = findWithDefault 0 a cs

-- 3ª definición
procesaTransaccion3 :: Transaccion -> Cuentas -> Cuentas
procesaTransaccion3 t =
  alter (add (- x)) (trDe t) .
  alter (add    x ) (trA  t)
  where x = trImporte t
        add a Nothing  = Just a
        add a (Just b) = Just (a + b)

-- Propiedad de equivalencia de las definiciones
prop_procesaTransaccion :: Transaccion -> Cuentas -> Bool
prop_procesaTransaccion t cs =
  all (== procesaTransaccion t cs)
      [procesaTransaccion2 t cs,
       procesaTransaccion3 t cs]

-- La comprobación es
--    λ> quickCheck prop_procesaTransaccion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    procesaTransacciones :: [Transaccion] -> Cuentas -> Cuentas
-- tal que (procesaTransacciones ts cs) es el estado de las cuentas
-- obtenido después de aplicar las transacciones ts a cs. Por ejemplo,
--    λ> t1 = Transaccion 60 "Ana" "Bea"
--    λ> t2 = Transaccion 40 "Bea" "Ana"
--    λ> t3 = Transaccion 40 "Bea" "Eva"
--    λ> procesaTransacciones [t1,t2,t3] (fromList [("Ana",90)])
--    fromList [("Ana",70),("Bea",-20),("Eva",40)]
-- ---------------------------------------------------------------------

procesaTransacciones :: [Transaccion] -> Cuentas -> Cuentas
procesaTransacciones []       cs = cs
procesaTransacciones (t : ts) cs =
  procesaTransacciones ts (procesaTransaccion t cs)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    procesaTransaccion' :: Transaccion -> Cuentas -> Maybe Cuentas
-- tal que (procesaTransaccion' t cs) es el estado de las cuentas
-- obtenido después de aplicar la transacción t a cs si no resulta
-- ningún saldo negativo y Nothing, en caso contrario. Por ejemplo,
--    λ> procesaTransaccion' (Transaccion 80 "Ana" "Bea") (fromList [("Ana",70)])
--    Nothing
--    λ> procesaTransaccion' (Transaccion 60 "Ana" "Bea") (fromList [("Ana",70)])
--    Just (fromList [("Ana",10),("Bea",60)])
--    λ> procesaTransaccion' (Transaccion 70 "Bea" "Ana") (fromList [("Ana",10),("Bea",60)])
--    Nothing
--    λ> procesaTransaccion' (Transaccion 40 "Bea" "Ana") (fromList [("Ana",10),("Bea",60)])
--    Just (fromList [("Ana",50),("Bea",20)])
-- ---------------------------------------------------------------------

procesaTransaccion' :: Transaccion -> Cuentas -> Maybe Cuentas
procesaTransaccion' (Transaccion i d a) cs
  | sd2 < 0 || sa2 < 0 = Nothing
  | otherwise          = Just (insert d sd2 (insert a sa2 cs))
  where sd1 = findWithDefault 0 d cs
        sa1 = findWithDefault 0 a cs
        sd2 = sd1 - i
        sa2 = sa1 + i

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    procesaTransacciones' :: [Transaccion] -> Cuentas -> Maybe Cuentas
-- tal que (procesaTransacciones' ts cs) es el estado de las cuentas
-- obtenido después de aplicar las transacciones ts a cs, si en el
-- proceso no hay saldos negativos y es Nothing, en caso contrario. Por
-- ejemplo,
--    λ> t1 = Transaccion 60 "Ana" "Bea"
--    λ> t2 = Transaccion 40 "Bea" "Ana"
--    λ> t3 = Transaccion 40 "Bea" "Eva"
--    λ> procesaTransacciones' [t1,t2] (fromList [("Ana",90)])
--    Just (fromList [("Ana",70),("Bea",20)])
--    λ> procesaTransacciones' [t1,t2,t3] (fromList [("Ana",90)])
--    Nothing
-- ---------------------------------------------------------------------

-- 1ª definición
procesaTransacciones' :: [Transaccion] -> Cuentas -> Maybe Cuentas
procesaTransacciones' []       cs = Just cs
procesaTransacciones' (t : ts) cs =
  case procesaTransaccion' t cs of
    Nothing  -> Nothing
    Just cs' -> procesaTransacciones' ts cs'


-- 2ª definición
procesaTransacciones'' :: [Transaccion] -> Cuentas -> Maybe Cuentas
procesaTransacciones'' []       cs = Just cs
procesaTransacciones'' (t : ts) cs =
  procesaTransaccion' t cs >>= procesaTransacciones'' ts

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Esta relación de ejercicios es una adaptación de
-- "Transactions.hs" https://bit.ly/3tBxSar de Lars Brünjes.
