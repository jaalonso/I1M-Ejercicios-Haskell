-- Analizadores_sintacticos.hs
-- Analizadores sintácticos.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Analizadores_sintacticos where

------------------------------------------------------------------------
-- § Introducción                                                     --
------------------------------------------------------------------------

-- En esta relación construiremos analizadores sintácticos, utilizando
-- las implementaciones estudiadas en el tema 12, cuyas transparencias
-- se encuentran en
--    https://jaalonso.github.io/cursos/i1m/temas/tema-12.html
--
-- Para realizar los ejercicios hay que tener instalada la librería de
-- I1M. Para instalarla basta ejecutar en una consola
--    cabal update
--    cabal install I1M

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import I1M.Analizador

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un número entero es un signo menos seguido por un número
-- natural o un número natural. Definir el analizador
--    int :: Analizador Int
-- para reconocer los números enteros. Por ejemplo,
--    analiza int "14DeAbril"   ==  [(14,"DeAbril")]
--    analiza int "-14DeAbril"  ==  [(-14,"DeAbril")]
-- ---------------------------------------------------------------------

int :: Analizador Int
int  = (caracter '-' >*> \_ ->
        nat          >*> \n ->
        resultado (-n))
       +++ nat

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir el analizador
--    comentario :: Analizador ()
-- para reconocer los comentarios simples de Haskell que comienzan con
-- el símbolo -- y terminan al final de la línea, que se representa por
-- el carácter de control '\n'. Por ejemplo,
--    λ> analiza comentario "-- 14DeAbril\nSiguiente"
--    [((),"Siguiente")]
--    λ> analiza comentario "- 14DeAbril\nSiguiente"
--    []
-- ---------------------------------------------------------------------

comentario :: Analizador ()
comentario = cadena "--"            >*> \_ ->
             varios (sat (/= '\n')) >*> \_ ->
             elemento               >*> \_ ->
             resultado ()

-- ---------------------------------------------------------------------
-- Ejercicio 3. Extender el analizador de expresiones aritméticas para
-- incluir restas y divisiones basándose en la siguiente extensión de
-- la gramática:
--    expr1 ::= term1 (+ expr1 | − expr1 | vacía)
--    term1 ::= factor1 (* term1 | / term1 | vacía)
-- Por ejemplo,
--    analiza expr1 "2*3+5"     ==  [(11,"")]
--    analiza expr1 "2*(3+5)"   ==  [(16,"")]
--    analiza expr1 "2+3*5"     ==  [(17,"")]
--    analiza expr1 "2*3+5abc"  ==  [(11,"abc")]
--    analiza expr1 "24/4-2"    ==  [(4,"")]
--    analiza expr1 "24/(4-2)"  ==  [(12,"")]
--    analiza expr1 "24-(4/2)"  ==  [(22,"")]
--    analiza expr1 "24/4-2abc" ==  [(4,"abc")]
-- ---------------------------------------------------------------------

expr1 :: Analizador Int
expr1 = term1                 >*> \t ->
        (simbolo "+"          >*> \_ ->
         expr1                >*> \e ->
         resultado (t+e))
        +++ (simbolo "-"      >*> \_ ->
             expr1            >*> \e ->
             resultado (t-e))
        +++ resultado t

-- term1 analiza un término de una expresión aritmética devolviendo su
-- valor. Por ejemplo,
--    analiza term1 "2*3+5"      ==  [(6,"+5")]
--    analiza term1 "2+3*5"      ==  [(2,"+3*5")]
--    analiza term1 "(2+3)*5+7"  ==  [(25,"+7")]
--    analiza term1 "2*3-6/3"    ==  [(6,"-6/3")]
--    analiza term1 "24/4-2"     ==  [(6,"-2")]
--    analiza term1 "24-4/2"     ==  [(24,"-4/2")]
--    analiza term1 "(24-4)/2+7" ==  [(10,"+7")]
--    analiza term1 "24/4-2^3"   ==  [(6,"-2^3")]
term1 :: Analizador Int
term1 =  factor1                     >*> \f ->
         (simbolo "*"                >*> \_ ->
          term1                      >*> \t ->
          resultado (f*t))
         +++ (simbolo "/"            >*> \_ ->
              term1                  >*> \t ->
              resultado (f `div` t))
         +++ resultado f

-- factor1 analiza un factor de una expresión aritmética devolviendo su
-- valor. Por ejemplo,
--   analiza factor1 "2*3+5"      ==  [(2,"*3+5")]
--   analiza factor1 "(2+3)*5"    ==  [(5,"*5")]
--   analiza factor1 "(2+3*7)*5"  ==  [(23,"*5")]
--   analiza factor1 "24/4-2"     ==  [(24,"/4-2")]
--   analiza factor1 "(24-4)/2"   ==  [(20,"/2")]
--   analiza factor1 "(24-4*2)/2" ==  [(16,"/2")]
factor1 :: Analizador Int
factor1 = (simbolo "("  >*> \_ ->
           expr1        >*> \e ->
           simbolo ")"  >*> \_ ->
           resultado e)
          +++ natural

-- ---------------------------------------------------------------------
-- Ejercicio 4. Extender el analizador de expresiones aritméticas para
-- incluir exponenciación, que asocie por la derecha y tenga mayor
-- prioridad que la multiplicación y la división, pero menor que los
-- paréntesis y los números. Por ejemplo,
--    analiza expr2 "2^3*4"  == [(32,"")]
-- Indicación: El nuevo nivel de prioridad requiere una nueva regla en
-- la gramática.
-- ---------------------------------------------------------------------

-- Las nuevas reglas son
--    factor2 ::= atomo (^ factor2 | epsilon)
--    atomo   ::= (expr) | nat

-- Las definiciones correspondientes son

-- expr2 analiza una expresión aritmética devolviendo su  valor. Por
-- ejemplo,
--    analiza expr2 "2*3+5"     ==  [(11,"")]
--    analiza expr2 "2*(3+5)"   ==  [(16,"")]
--    analiza expr2 "2+3*5"     ==  [(17,"")]
--    analiza expr2 "2*3+5abc"  ==  [(11,"abc")]
--    analiza expr2 "24/4-2"    ==  [(4,"")]
--    analiza expr2 "24/(4-2)"  ==  [(12,"")]
--    analiza expr2 "24-(4/2)"  ==  [(22,"")]
--    analiza expr2 "24/4-2abc" ==  [(4,"abc")]
--    analiza expr2 "2^3*4"     == [(32,"")]
expr2 :: Analizador Int
expr2 = term2                  >*> \t ->
         (simbolo "+"          >*> \_ ->
          expr2                >*> \e ->
          resultado (t+e))
         +++ (simbolo "-"      >*> \_ ->
              expr2            >*> \e ->
              resultado (t-e))
         +++ resultado t

-- term2 analiza un término de una expresión aritmética devolviendo su
-- valor. Por ejemplo,
--    analiza term2 "2*3+5"      ==  [(6,"+5")]
--    analiza term2 "2+3*5"      ==  [(2,"+3*5")]
--    analiza term2 "(2+3)*5+7"  ==  [(25,"+7")]
--    analiza term2 "2*3-6/3"    ==  [(6,"-6/3")]
--    analiza term2 "24/4-2"     ==  [(6,"-2")]
--    analiza term2 "24-4/2"     ==  [(24,"-4/2")]
--    analiza term2 "(24-4)/2+7" ==  [(10,"+7")]
--    analiza term2 "24/4-2^3"   ==  [(6,"-2^3")]
--    analiza term2 "2^3*4"      == [(32,"")]
term2 :: Analizador Int
term2 = factor2                      >*> \f ->
         (simbolo "*"                >*> \_ ->
          term2                      >*> \t ->
          resultado (f*t))
         +++ (simbolo "/"            >*> \_ ->
              term2                  >*> \t ->
              resultado (f `div` t))
         +++ resultado f

-- factor2 analiza un factor de una expresión aritmética devolviendo su
-- valor. Por ejemplo,
--   analiza factor2 "2*3+5"      ==  [(2,"*3+5")]
--   analiza factor2 "(2+3)*5"    ==  [(5,"*5")]
--   analiza factor2 "(2+3*7)*5"  ==  [(23,"*5")]
--   analiza factor2 "24/4-2"     ==  [(24,"/4-2")]
--   analiza factor2 "(24-4)/2"   ==  [(20,"/2")]
--   analiza factor2 "(24-4*2)/2" ==  [(16,"/2")]
--   analiza factor2 "2^3*4"      ==  [(8,"*4")]
factor2 :: Analizador Int
factor2 = (atomo >*> \a ->
            (simbolo "^" >*> \_ ->
             factor2     >*> \f ->
             resultado (a ^ f))
            +++ resultado a)

-- atomo analiza un átomo de una expresión aritmética devolviendo su
-- valor. Por ejemplo,
--    analiza atomo "2^3*4"    ==  [(2,"^3*4")]
--    analiza atomo "(2^3)*4"  ==  [(8,"*4")]
atomo :: Analizador Int
atomo = (simbolo "("  >*> \_ ->
         expr2        >*> \e ->
         simbolo ")"  >*> \_ ->
         resultado e)
        +++ natural

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir el analizador
--    expr3 :: Analizador Arbol
-- tal que (analiza expr3 c) es el árbol e la expresión correspondiente
-- a la cadena c. Por ejemplo,
--    λ> analiza expr3 "2*3+5"
--    [(N '+' (N '*' (H 2) (H 3)) (H 5),"")]
--    λ> analiza expr3 "2*(3+5)"
--    [(N '*' (H 2) (N '+' (H 3) (H 5)),"")]
--    λ> analiza expr3 "2+3*5"
--    [(N '+' (H 2) (N '*' (H 3) (H 5)),"")]
--    λ> analiza expr3 "2*3+5abc"
--    [(N '+' (N '*' (H 2) (H 3)) (H 5),"abc")]
-- ---------------------------------------------------------------------

data Arbol = H Int | N Char Arbol Arbol
             deriving Show

expr3 :: Analizador Arbol
expr3 = term3 >*> \t ->
        (simbolo "+"  >*> \_ ->
         expr3        >*> \e ->
         resultado (N '+' t e))
        +++ resultado t

-- analiza term3 "2*3+5"  ==  [(N '*' (H 2) (H 3),"+5")]
term3 :: Analizador Arbol
term3 = factor3 >*> \f ->
        (simbolo "*" >*> \_ ->
         term3       >*> \t ->
         resultado (N '*' f t))
        +++ resultado f

-- analiza factor3 "2*3+5"  ==  [(H 2,"*3+5")]
factor3 :: Analizador Arbol
factor3 = (simbolo "(" >*> \_ ->
           expr3       >*> \e ->
           simbolo ")" >*> \_ ->
           resultado e)
          +++ natural'

-- analiza nat3 "14DeAbril"  ==  [(H 14,"DeAbril")]
-- analiza nat3 " 14DeAbril"  ==  []
nat3 :: Analizador Arbol
nat3 = varios1 digito >*> \xs ->
       resultado (H (read xs))

-- analiza natural' "  14DeAbril"  ==  [(H 14,"DeAbril")]
natural' :: Analizador Arbol
natural' =  unidad nat3

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función
--    arbolAnalisis :: String -> Arbol
-- tal que (arbolAnalisis c) es el árbol de análisis correspondiente a
-- la cadena c, si c representa a una expresión aritmética y error en
-- caso contrario. Por ejemplo,
--    λ> arbolAnalisis "2*3+5"
--    N '+' (N '*' (H 2) (H 3)) (H 5)
--    λ> arbolAnalisis "2*(3+5)"
--    N '*' (H 2) (N '+' (H 3) (H 5))
--    λ> arbolAnalisis "2 * 3 + 5"
--    N '+' (N '*' (H 2) (H 3)) (H 5)
--    λ> arbolAnalisis "2*3x+5y"
--    *** Exception: entrada sin usar x+5y
--    λ> arbolAnalisis "-1"
--    *** Exception: entrada no valida
-- ---------------------------------------------------------------------

arbolAnalisis :: String -> Arbol
arbolAnalisis xs = case (analiza expr3 xs) of
                     [(t,[])]  -> t
                     [(_,sal)] -> error ("entrada sin usar " ++ sal)
                     []        -> error "entrada no valida"
                     _         -> error "Imposible"

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    listaNV :: Analizador a -> Analizador [a]
-- tal que (listaNV p) es un analizador de listas no vacías de elementos
-- reconocibles por el analizador p. Por ejemplo,
--    λ> analiza (listaNV natural) "[3, 5,4]"
--    [([3,5,4],"")]
--    λ> analiza (listaNV natural) "[3, 5,4.0]"
--    []
--    λ> analiza (listaNV identificador) "[hoy , es,lunes ]"
--    [(["hoy","es","lunes"],"")]
--    λ> analiza (listaNV identificador) "[hoy , es,lunes,18 ]"
--    []
-- ---------------------------------------------------------------------

listaNV :: Analizador a -> Analizador [a]
listaNV p = simbolo "["          >*> \_ ->
            p                    >*> \x ->
            varios (simbolo ","  >*> \_ ->
                    p)           >*> \xs ->
            simbolo "]"          >*> \_ ->
            resultado (x:xs)

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir el analizador
--    exprPBA :: Analizador ()
-- para reconocer cadenas de paréntesis bien anidados. Por ejemplo,
--    analiza exprPBA "(())()"  ==  [((),"")]
--    analiza exprPBA "(()))("  ==  [((),")(")]
-- ---------------------------------------------------------------------

-- La gramática es
--    exprPBA := '(' exprPBA ')' exprPBA | vacía

exprPBA :: Analizador ()
exprPBA = (simbolo "("  >*> \_i  ->
           exprPBA     >*> \_xs ->
           simbolo ")"  >*> \_f  ->
           exprPBA     >*> \_ys ->
           resultado ())
          +++
          (simbolo "" >*> \_ ->
           resultado ())

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir el analizador
--    exprPBA :: Analizador ()
-- para reconocer simbolos de paréntesis bien anidados con cálculo de la
-- mayor profundidad de anidamiento. Por ejemplo,
--    analiza exprPBA2 ""          ==  [(0,"")]
--    analiza exprPBA2 "()"        ==  [(1,"")]
--    analiza exprPBA2 "()()"      ==  [(1,"")]
--    analiza exprPBA2 "(())()"    ==  [(2,"")]
--    analiza exprPBA2 "((())())"  ==  [(3,"")]
--    analiza exprPBA2 "())("      ==  [(1,")(")]
-- ---------------------------------------------------------------------

exprPBA2 :: Analizador Int
exprPBA2 = (simbolo "("   >*> \_  ->
            exprPBA2     >*> \n ->
            simbolo ")"   >*> \_  ->
            exprPBA2     >*> \m ->
            resultado (max (n+1) m))
           +++
           (simbolo "" >*> \_ ->
            resultado 0)
