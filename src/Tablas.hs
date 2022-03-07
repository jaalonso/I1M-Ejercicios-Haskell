-- Tablas.hs
-- Tablas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- En esta relación se define el tipo abstracto de dato (TAD) de las tablas
-- como lista de asociación de claves y valores. Los procedimientos del
-- TAD son
--    vacia         :: Tabla k v
--    inserta       :: k -> v -> Tabla k v -> Tabla k v
--    borra         :: Eq k => k -> Tabla k v -> Tabla k v
--    busca         :: Eq k => k -> Tabla k v -> Maybe v
--    aplicaValores :: (v1 -> v2) -> Tabla k v1 -> Tabla k v2
--    aplicaClaves  :: (k1 -> k2) -> Tabla k1 v -> Tabla k2 v
--    ajusta        :: Eq k => (Maybe v -> Maybe v) -> k -> Tabla k v -> Tabla k v
-- En la siguiente relación se comprueba con QuickCheck cómo las
-- anteriores funciones de la tablas se corresponden con funciones de
-- diccionarios de la libreria Data.Map.

module Tablas
  ( Tabla (..)
  , vacia, inserta, borra, busca, aplicaValores, aplicaClaves, ajusta
  )
where

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir el tipo (Tabla k v) de las tablas con claves de
-- tipo k y valores de tipo v.
-- ---------------------------------------------------------------------

newtype Tabla k v = Tabla [(k, v)]
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    vacia :: Tabla k v
-- tal que vacia es la tabla vacía.
-- ---------------------------------------------------------------------

vacia :: Tabla k v
vacia = Tabla []

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    inserta :: k -> v -> Tabla k v -> Tabla k v
-- tal que
--    inserta 2 'a' vacia                 == Tabla [(2,'a')]
--    inserta 4 'd' (inserta 2 'a' vacia) == Tabla [(4,'d'),(2,'a')]
-- ---------------------------------------------------------------------

inserta :: k -> v -> Tabla k v -> Tabla k v
inserta k v (Tabla kvs) = Tabla ((k, v) : kvs)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    borra :: Eq k => k -> Tabla k v -> Tabla k v
-- tal que (borra k t) es la tabla obtenida borrando los pares de t
-- cuya clave es k. Por ejemplo,
--    λ> borra 2 (Tabla [(2,'a'),(3,'b'),(2,'a')])
--    Tabla [(3,'b')]
-- ---------------------------------------------------------------------

borra :: Eq k => k -> Tabla k v -> Tabla k v
borra k (Tabla kvs) = Tabla $ filter (\(k', _) -> k' /= k) kvs

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    busca :: Eq k => k -> Tabla k v -> Maybe v
-- tal que (busca k t) es el valor de la clave k en la tabla t. Por ejemplo,
--    busca 2 (Tabla [(2,'a'),(3,'b'),(2,'d')])  ==  Just 'a'
--    busca 4 (Tabla [(2,'a'),(3,'b'),(2,'d')])  ==  Nothing
-- ---------------------------------------------------------------------

busca :: Eq k => k -> Tabla k v -> Maybe v
busca _ (Tabla [])              = Nothing
busca k (Tabla ((k', v) : kvs))
  | k == k'                      = Just v
  | otherwise                    = busca k (Tabla kvs)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    aplicaValores :: (v1 -> v2) -> Tabla k v1 -> Tabla k v2
-- tal que (aplicaValores f t) es la tabla obtenida aplicando la función f a
-- los valores de t. Por ejemplo,
--    λ> aplicaValores (+2) (Tabla [('a',5),('b',7),('c',4)])
--    Tabla [('a',7),('b',9),('c',6)]
-- ---------------------------------------------------------------------

aplicaValores :: (v1 -> v2) -> Tabla k v1 -> Tabla k v2
aplicaValores _ (Tabla [])             = vacia
aplicaValores f (Tabla ((k, v) : kvs)) = inserta k (f v) (aplicaValores f (Tabla kvs))

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    aplicaClaves :: (k1 -> k2) -> Tabla k1 v -> Tabla k2 v
-- tal que (aplicaClaves f t) es la tabla obtenida aplicando la función f a
-- las claves de t. Por ejemplo,
--    λ> aplicaClaves (+2) (Tabla [(2,'a'),(3,'b'),(2,'d')])
--    Tabla [(4,'a'),(5,'b'),(4,'d')]
-- ---------------------------------------------------------------------

aplicaClaves :: (k1 -> k2) -> Tabla k1 v -> Tabla k2 v
aplicaClaves _ (Tabla [])             = vacia
aplicaClaves f (Tabla ((k, v) : kvs)) = inserta (f k) v (aplicaClaves f (Tabla kvs))

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    ajusta :: Eq k => (Maybe v -> Maybe v) -> k -> Tabla k v -> Tabla k v
-- tal que (ajusta f k t) es el ajuste de la tabla t de acuerdo a las
-- siguientes reglas:
-- + Si `f k` es `Nothing` y `k` es una clave de t, borra el par con
--   clave `k`.
-- + Si `f k` es `Nothing` y `k` no es una clave de t, no hace nada.
-- + Si `f k` es `Just v` y `k` es una clave de t, cambia el valor de
--   `k` a `v`.
-- + Si `f k` es `Just v` y `k` no es una clave de t, añade el par `(h, v)`.
-- Por ejemplo,
--    λ> ajusta (\_ -> Nothing) 4 (Tabla [(3,2),(4,5)])
--    Tabla [(3,2)]
--    λ> ajusta (\_ -> Nothing) 7 (Tabla [(3,2),(4,5)])
--    Tabla [(3,2),(4,5)]
--    λ> ajusta (\_ -> Just 9) 4 (Tabla [(3,2),(4,5)])
--    Tabla [(3,2),(4,9)]
--    λ> ajusta (\_ -> Just 9) 7 (Tabla [(3,2),(4,5)])
--    Tabla [(3,2),(4,5),(7,9)]
--    λ> ajusta ((+ 2) <$>) 4 (Tabla [(3,2),(4,5)])
--    Tabla [(3,2),(4,7)]
--    λ> ajusta ((+ 2) <$>) 7 (Tabla [(3,2),(4,5)])
--    Tabla [(3,2),(4,5)]
--    λ> ajusta (\_ -> Nothing) 3 (Tabla [])
--    Tabla []
--    λ> ajusta (\_ -> Just 7) 3 (Tabla [])
--    Tabla [(3,7)]
--    λ> ajusta (\_ -> Nothing) 3 (Tabla [(3,1),(2,5),(3,7),(4,3)])
--    Tabla [(2,5),(4,3)]
--    λ> ajusta ((+ 2) <$>) 3 (Tabla [(3,1),(2,5),(3,7),(4,3)])
--    Tabla [(3,3),(2,5),(3,7),(4,3)]
--    λ> ajusta (\_ -> Nothing) 3 (Tabla [(2,5),(3,7),(4,3)])
--    Tabla [(2,5),(4,3)]
--    λ> ajusta ((+ 2) <$>) 3 (Tabla [(2,5),(3,7),(4,3)])
--    Tabla [(2,5),(3,9),(4,3)]
-- ---------------------------------------------------------------------

ajusta :: Eq k => (Maybe v -> Maybe v) -> k -> Tabla k v -> Tabla k v
ajusta f k (Tabla []) =
  case f Nothing of
    Nothing -> Tabla []
    Just v  -> Tabla [(k, v)]
ajusta f k (Tabla ((k', v) : kvs))
  | k == k' =
      case f (Just v) of
        Nothing -> Tabla $ filter (\(k'', _) -> k'' /= k) kvs
        Just v' -> Tabla $ (k, v') : kvs
  | otherwise =
      case ajusta f k (Tabla kvs) of
        Tabla kvs' -> Tabla $ (k', v) : kvs'

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Esta relación de ejercicio es una adaptación de
-- "Tables.hs" https://bit.ly/3Cli8vV de Lars Brünjes.
