-- Cadenas_de_bloques.hs
-- Cadenas de bloques.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-febrero-2022
-- ---------------------------------------------------------------------

module Cadenas_de_bloques where

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es presentar una
-- modelización elemental de las cadenas de bloques y usarla para
-- presentar los métodos generales de definiciones de funciones en
-- Haskell.
--
-- Según la Wikipedia, una [cadena de bloques](https://bit.ly/35LzyWh),
-- en inglés "blockchain", es una estructura de datos cuya información
-- se agrupa en bloques a los que se les añade metainformaciones
-- relativas a otro bloque de la cadena anterior en una línea temporal.
--
-- El tipo de datos Cadenas representa las dacenas de bloque. Tiene un
-- argumento que representa la transacción del bloque anterior al
-- actual. Posee dos constructores:
-- + BloqueOriginal que es el bloque con que se inicia la cadena y
-- + Bloque que a partir de una cadena c y una transacción t construye una
--   nueva cadena añadiéndole a c un bloque con la transacción t.
-- Además, se derivan las clases Eq y Show.

data Cadena t = BloqueOriginal
              | Bloque (Cadena t) t
  deriving (Eq, Show)

-- Para simplificar la notación, se define el operador (|>) como el
-- constructor Bloque.
(|>) :: Cadena t -> t -> Cadena t
(|>) = Bloque

infixl 5 |>

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    longitudCadena :: Cadena t -> Int
-- tal que (longitudCadena c) es la longitud de la cadena c. Por ejemplo,
--    longitudCadena (BloqueOriginal |>2 |>5 |>2)  ==  3
-- ---------------------------------------------------------------------

longitudCadena :: Cadena t -> Int
longitudCadena BloqueOriginal = 0
longitudCadena (Bloque c _)   = 1 + longitudCadena c

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    sumaCadena :: Cadena Int -> Int
-- tal que (sumaCadena c) es la suma de las transacciones de la cadena
-- c. Por ejemplo,
--    sumaCadena (BloqueOriginal |>2 |>5 |>2)  ==  9
-- ---------------------------------------------------------------------

sumaCadena :: Cadena Int -> Int
sumaCadena BloqueOriginal = 0
sumaCadena (Bloque c tx) = tx + sumaCadena c

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    maxCadena :: Cadena Int -> Int
-- tal que (maxCadena c) es la mayor de las transacciones de la cadena
-- c. Por ejemplo,
--    maxCadena (BloqueOriginal |>2 |>5 |>2)  ==  5
-- ---------------------------------------------------------------------

maxCadena :: Cadena Int -> Int
maxCadena BloqueOriginal = 0
maxCadena (Bloque c tx) = tx `max` maxCadena c

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    cadenaMasLarga :: Cadena t -> Cadena t -> Cadena t
-- tal que (cadenaMasLarga c d) es la cadena de mayor longitud o la
-- primera, si las dos tienen la misma longitud. Por ejemplo,
--    λ> cadenaMasLarga (BloqueOriginal |>7) (BloqueOriginal |>2 |>1)
--    Bloque (Bloque BloqueOriginal 2) 1
--    λ> cadenaMasLarga (BloqueOriginal |>2 |>1) (BloqueOriginal |>7)
--    Bloque (Bloque BloqueOriginal 2) 1
--    λ> cadenaMasLarga (BloqueOriginal |>2) (BloqueOriginal |>7)
--    Bloque BloqueOriginal 2
-- ---------------------------------------------------------------------

cadenaMasLarga :: Cadena t -> Cadena t -> Cadena t
cadenaMasLarga c d
  | longitudCadena c >= longitudCadena d = c
  | otherwise                            = d

-- ---------------------------------------------------------------------
-- Ejercicio 5. Se dice que una cadena es válida si, desde el inicio,
-- cada transacción es mayor que todas las precedentes.
--
-- Definir la función
--    cadenaValida :: Cadena Int -> Bool
-- tal que (cadenaValida c) se verifica si c es válida. Por ejemplo,
--    cadenaValida (BloqueOriginal |>3 |>6 |>7)  ==  True
--    cadenaValida (BloqueOriginal |>3 |>3 |>7)  ==  False
--    cadenaValida (BloqueOriginal |>3 |>2 |>7)  ==  False
-- ---------------------------------------------------------------------

cadenaValida :: Cadena Int -> Bool
cadenaValida BloqueOriginal              = True
cadenaValida (Bloque BloqueOriginal _)   = True
cadenaValida (Bloque c@(Bloque _ t1) t2) = t2 > t1 && cadenaValida c

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    esPrefijoDe :: Eq t => Cadena t -> Cadena t -> Bool
-- tal que (esPrefijoDe c1 c2) se verifica si c1 es un prefijo de c2 o si
-- son iguales. Por ejemplo,
--    λ> (BloqueOriginal |>1 |>3) `esPrefijoDe` (BloqueOriginal |>1 |>3 |>2)
--    True
--    λ> (BloqueOriginal |>1 |>3) `esPrefijoDe` (BloqueOriginal |>1 |>2 |>3)
--    False
--    λ> (BloqueOriginal |>1 |>3) `esPrefijoDe` (BloqueOriginal |>1 |>3)
--    True
-- ---------------------------------------------------------------------

esPrefijoDe :: Eq t => Cadena t -> Cadena t -> Bool
esPrefijoDe BloqueOriginal BloqueOriginal = True
esPrefijoDe (Bloque _ _)   BloqueOriginal = False
esPrefijoDe c              d@(Bloque e _) = c `esPrefijoDe` e || c == d

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    sonCompatibles :: Eq t => Cadena t -> Cadena t -> Bool
-- tal que (sonCompatibles c d) se verifica cuando una es prefijo de la
-- otra. Por ejemplo,
--    λ> sonCompatibles (BloqueOriginal |>3) (BloqueOriginal |>3 |>2 |>1)
--    True
--    λ> sonCompatibles (BloqueOriginal |>3 |>2 |>1) (BloqueOriginal |>3)
--    True
--    λ> sonCompatibles (BloqueOriginal |>2 |>1) (BloqueOriginal |>3)
--    False
-- ---------------------------------------------------------------------

sonCompatibles :: Eq t => Cadena t -> Cadena t -> Bool
sonCompatibles c d = c `esPrefijoDe` d || d `esPrefijoDe` c

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    prefijoComun :: Eq t => Cadena t -> Cadena t -> Cadena t
-- tal que (prefijoComun c d) es el mayor prefijo común a c y d. Por
-- ejemplo,
--    λ> prefijoComun (BloqueOriginal |>3 |>2 |>5) (BloqueOriginal |>3 |>2 |>7)
--    Bloque (Bloque BloqueOriginal 3) 2
--    λ> prefijoComun (BloqueOriginal |>3 |>5 |>7) (BloqueOriginal |>3 |>2 |>7)
--    Bloque BloqueOriginal 3
--    λ> prefijoComun (BloqueOriginal |>4 |>5 |>7) (BloqueOriginal |>3 |>2 |>7)
--    BloqueOriginal
-- ---------------------------------------------------------------------

prefijoComun :: Eq t => Cadena t -> Cadena t -> Cadena t
prefijoComun BloqueOriginal  _ = BloqueOriginal
prefijoComun c@(Bloque d _) e
  | c `esPrefijoDe` e = c
  | otherwise         = prefijoComun d e

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    tieneBloqueProp :: (t -> Bool) -> Cadena t -> Bool
-- tal que (tieneBloqueProp p c) se verifica si alguna transacción de c
-- cumple la propiedad p. Por ejemplo,
--    tieneBloqueProp even (BloqueOriginal |>3 |>2 |>5)  ==  True
--    tieneBloqueProp even (BloqueOriginal |>3 |>7 |>5)  ==  False
-- ---------------------------------------------------------------------

tieneBloqueProp :: (t -> Bool) -> Cadena t -> Bool
tieneBloqueProp _ BloqueOriginal = False
tieneBloqueProp p (Bloque c t) = p t || tieneBloqueProp p c

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    tieneBloque :: Eq t => t -> Cadena t -> Bool
-- tal que (tieneBloque t c) se verifica si alguna transacción de c es
-- igual a t. Por ejemplo,
--    tieneBloque 7 (BloqueOriginal |>3 |>7 |>5)  ==  True
--    tieneBloque 8 (BloqueOriginal |>3 |>7 |>5)  ==  False
-- ---------------------------------------------------------------------

tieneBloque :: Eq t => t -> Cadena t -> Bool
tieneBloque t = tieneBloqueProp (== t)

-- ---------------------------------------------------------------------
-- Ejercicio 11. Defin9ir la función
--    bloquesUnicos :: Eq t => Cadena t -> Bool
-- tal que (bloquesUnicos c) se verifica si todos los bloque de c son
-- únicos (es decir, sus transacciones son distintas). Por ejemplo,
--    bloquesUnicos (BloqueOriginal |>3 |>7 |>5)  ==  True
--    bloquesUnicos (BloqueOriginal |>3 |>7 |>3)  ==  False
-- ---------------------------------------------------------------------

bloquesUnicos :: Eq t => Cadena t -> Bool
bloquesUnicos BloqueOriginal = True
bloquesUnicos (Bloque c t) = bloquesUnicos c && not (tieneBloque t c)

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    todosBloquesProp :: (t -> Bool) -> Cadena t -> Bool
-- tal que (todosBloquesProp p c) se verifica si todos los bloques de c
-- cumplen la propiedad p. Por ejemplo,
--    todosBloquesProp (== 'x') BloqueOriginal == True
--    todosBloquesProp even cadena2           == True
--    todosBloquesProp even cadena3           == False
-- ---------------------------------------------------------------------

todosBloquesProp :: (t -> Bool) -> Cadena t -> Bool
todosBloquesProp _ BloqueOriginal = True
todosBloquesProp p (Bloque c t) = p t && todosBloquesProp p c

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    maxCadenas :: [Cadena t] -> Int
-- tal que (maxCadenas cs) es el máximo de las longitudes de las cadenas
-- de cs. Por ejemplo,
--    λ> c1 = BloqueOriginal |>3
--    λ> c2 = BloqueOriginal |>5 |>1
--    λ> c3 = BloqueOriginal |>2 |>1 |>2
--    λ> maxCadenas [c1, c2, c3]
--    3
-- ---------------------------------------------------------------------

maxCadenas :: [Cadena t] -> Int
maxCadenas []       = 0
maxCadenas (c : cs) = longitudCadena c `max` maxCadenas cs

-- Se puede definir con foldr
maxCadenas' :: [Cadena t] -> Int
maxCadenas' = foldr (max . longitudCadena) 0

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    mayorPrefijoComun :: Eq t => [Cadena t] -> Cadena t
-- tal que (mayorPrefijoComun c cs) es el mayor prefijo común de las
-- cadenas c y las de cs. Por ejemplo,
--    λ> c1 = BloqueOriginal |>3 |>5 |>7 |>4
--    λ> c2 = BloqueOriginal |>3 |>5 |>2
--    λ> c3 = BloqueOriginal |>5 |>2
--    λ> mayorPrefijoComun [c1, c2]
--    Bloque (Bloque BloqueOriginal 3) 5
--    λ> mayorPrefijoComun [c1, c2, c3]
--    BloqueOriginal
-- ---------------------------------------------------------------------

mayorPrefijoComun :: Eq t => [Cadena t] -> Cadena t
mayorPrefijoComun []       = BloqueOriginal
mayorPrefijoComun [c]      = c
mayorPrefijoComun (c : cs) = c `prefijoComun` mayorPrefijoComun cs

-- ---------------------------------------------------------------------
-- Ejercicio 15. Dada una cadena de enteros, se interpreta cada entero
-- como un cambio del saldo actual. El bloque inicial tiene un saldo de
-- 0. El saldo final viene dado por sumaCadena.
--
-- Definir la función
--    balancesCadena :: Cadena Int -> Cadena Int
-- tal que (balancesCadena c) es la cadena de los saldos intermedios (es
-- decir, una cadena con la  misma longitud que c, pero cada entrada
-- debe ser el saldo intermedio de la cadena original en ese punto). Por
-- ejemplo,
--    λ> balancesCadena (BloqueOriginal |>2 |>8 |>4)
--    Bloque (Bloque (Bloque BloqueOriginal 2) 10) 14
-- ---------------------------------------------------------------------

balancesCadena :: Cadena Int -> Cadena Int
balancesCadena BloqueOriginal = BloqueOriginal
balancesCadena (Bloque c t) =
  case balancesCadena c of
    BloqueOriginal -> Bloque BloqueOriginal t
    d@(Bloque _ b) -> Bloque d (b + t)

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    cadenaSinSaldosNegativos :: Cadena Int -> Bool
-- tal que (cadenaSinSaldosNegativos) se verifica si ninnguno de los saldos
-- intermedios de c es negativo. Por ejemplo,
--    cadenaSinSaldosNegativos (BloqueOriginal |>2 |>8 |>4)    == True
--    cadenaSinSaldosNegativos (BloqueOriginal |>2 |>(-1) |>4) == True
--    cadenaSinSaldosNegativos (BloqueOriginal |>2 |>(-3) |>4) == False
-- ---------------------------------------------------------------------

cadenaSinSaldosNegativos :: Cadena Int -> Bool
cadenaSinSaldosNegativos = todosBloquesProp (>= 0) . balancesCadena

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    acortaMientras :: (t -> Bool) -> Cadena t -> Cadena t
-- tal que (acortaMientras p cs) es la cadena obtenida eliminando los
-- bloques finales de c que cumplen la propiedad p. Por ejemplo,
--    λ> acortaMientras even (BloqueOriginal |>2 |>3 |>4 |>6)
--    Bloque (Bloque BloqueOriginal 2) 3
--    λ> acortaMientras even (BloqueOriginal |>2 |>8 |>4 |>6)
--    BloqueOriginal
--    λ> acortaMientras even (BloqueOriginal |>2 |>8 |>4 |>5)
--    Bloque (Bloque (Bloque (Bloque BloqueOriginal 2) 8) 4) 5
-- ---------------------------------------------------------------------

acortaMientras :: (t -> Bool) -> Cadena t -> Cadena t
acortaMientras _ BloqueOriginal = BloqueOriginal
acortaMientras p c@(Bloque d t)
  | p t       = acortaMientras p d
  | otherwise = c

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    construyeCadena :: Int -> Cadena Int
-- tal que (construyeCadena n) es la cadena con n bloques donde las transacciones
-- son 1, 2,..., n. Por ejemplo,
--    λ> construyeCadena 4
--    Bloque (Bloque (Bloque (Bloque BloqueOriginal 1) 2) 3) 4
-- ---------------------------------------------------------------------

construyeCadena :: Int -> Cadena Int
construyeCadena n
  | n <= 0    = BloqueOriginal
  | otherwise = Bloque (construyeCadena (n - 1)) n

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    replicaCadena :: Int -> t -> Cadena t
-- tal que (replicaCadena n t) es la cadena con n bloques cada uno con
-- la transacción t. Por ejemplo,
--    λ> replicaCadena 3 7
--    Bloque (Bloque (Bloque BloqueOriginal 7) 7) 7
-- ---------------------------------------------------------------------

replicaCadena :: Int -> t -> Cadena t
replicaCadena n t
  | n <= 0    = BloqueOriginal
  | otherwise = Bloque (replicaCadena (n - 1) t) t

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    prefijo :: Int -> Cadena t -> Cadena t
-- tal que (prefijo n c) es la cadena formada por los n primeros
-- bloques de c. Por ejemplo,
--    λ> prefijo 2 (BloqueOriginal |> 3 |> 7 |> 5 |> 4)
--    Bloque (Bloque BloqueOriginal 3) 7
--    λ> prefijo 5 (BloqueOriginal |> 3 |> 7 |> 5 |> 4)
--    Bloque (Bloque (Bloque (Bloque BloqueOriginal 3) 7) 5) 4
--    λ> prefijo (-3) (BloqueOriginal |> 3 |> 7 |> 5 |> 4)
--    BloqueOriginal
-- ---------------------------------------------------------------------

prefijo :: Int -> Cadena t -> Cadena t
prefijo _ BloqueOriginal   = BloqueOriginal
prefijo n c@(Bloque d _)
  | n >= longitudCadena c = c
  | otherwise             = prefijo n d

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Esta relación de ejercicio es una adaptación de la de Lars Brünjes
-- "Chain.hs" https://bit.ly/3IHrdBX
