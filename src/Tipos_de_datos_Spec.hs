module Tipos_de_datos_Spec (main, spec) where

import           Prelude hiding (or, reverse)
import qualified Prelude as P
import           Test.Hspec
import           Test.QuickCheck

import           Tipos_de_datos as E

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "implicacion" $ do
    it "e1" $
      implicacion False False  `shouldBe`  True
    it "e2" $
      implicacion True False   `shouldBe`  False
    it "p1" $
      property prop_implicacion_implicacion'
  describe "orelse" $ do
    it "e1" $
      (Nothing `orelse` Nothing) `shouldBe` (Nothing :: Maybe ())
    it "e2" $
      (Nothing `orelse` Just 5)  `shouldBe` Just 5
    it "e3" $
      (Just 3  `orelse` Nothing) `shouldBe` Just 3
    it "e4" $
      (Just 3  `orelse` Just 5)  `shouldBe` Just 3
  describe "mapMaybe" $ do
    it "e1" $
      mapMaybe (+ 2) (Just 6)  `shouldBe`  Just 8
    it "e2" $
      mapMaybe (+ 2) Nothing   `shouldBe`  Nothing
  describe "orelse'" $ do
    it "e1" $
      (Nothing `orelse'` Nothing) `shouldBe` (Nothing :: Maybe ())
    it "e2" $
      (Nothing `orelse'` Just 5)  `shouldBe` Just 5
    it "e3" $
      (Just 3  `orelse'` Nothing) `shouldBe` Just 3
    it "e4" $
      (Just 3  `orelse'` Just 5)  `shouldBe` Just 3
    it "p1" $
      property prop_orelse_orelse'
  describe "mapMaybe'" $ do
    it "e1" $
      mapMaybe' (+ 2) (Just 6)  `shouldBe`  Just 8
    it "e2" $
      mapMaybe' (+ 2) Nothing   `shouldBe`  Nothing
  describe "parMaybe" $ do
    it "e1" $
      parMaybe (Just 'x') (Just 'y')  `shouldBe`  Just ('x','y')
    it "e2" $
      parMaybe (Just 42) Nothing      `shouldBe`  (Nothing :: Maybe (Int, Int))
    it "p1" $
      property prop_parMaybe_parMaybe'
  describe "liftMaybe" $ do
    it "e1" $
      liftMaybe (*)  (Just 2)    (Just 3)      `shouldBe`  Just 6
    it "e2" $
      liftMaybe (*)  (Just 2)    Nothing       `shouldBe`  (Nothing :: Maybe Int)
    it "e3" $
      liftMaybe (P.++) (Just "ab") (Just "cd") `shouldBe`  Just "abcd"
    it "e4" $
      liftMaybe elem (Just 'b')  (Just "abc")  `shouldBe`  Just True
    it "e5" $
      liftMaybe elem (Just 'p')  (Just "abc")  `shouldBe`  Just False
  describe "addMaybes" $ do
    it "e1" $
      (Just 2  `sumaMaybes` Just 3)  `shouldBe` Just 5
    it "e2" $
      (Just 2  `sumaMaybes` Nothing) `shouldBe` (Nothing :: Maybe Int)
    it "e3" $
      (Nothing `sumaMaybes` Just 3)  `shouldBe` (Nothing :: Maybe Int)
    it "e4" $
      (Nothing `sumaMaybes` Nothing) `shouldBe` (Nothing :: Maybe Int)
    it "behaves like addMaybes" $
      property prop_sumaMaybes_sumaMaybes'
  describe "liftMaybe'" $ do
    it "e1" $
      liftMaybe' (*)  (Just 2)    (Just 3)      `shouldBe`  Just 6
    it "e2" $
      liftMaybe' (*)  (Just 2)    Nothing       `shouldBe`  (Nothing :: Maybe Int)
    it "e3" $
      liftMaybe' (P.++) (Just "ab") (Just "cd") `shouldBe`  Just "abcd"
    it "e4" $
      liftMaybe' elem (Just 'b')  (Just "abc")  `shouldBe`  Just True
    it "e5" $
      liftMaybe' elem (Just 'p')  (Just "abc")  `shouldBe`  Just False
  describe "aplicaAmbas" $
    it "e1" $
      aplicaAmbas (+ 1) (* 2) 7  `shouldBe`  (8,14)
  describe "(++)" $ do
    it "e1" $
      [2,3] E.++ [4,5,1]  `shouldBe`  [2,3,4,5,1]
    it "p1" $
      property prop_conc
  describe "or" $ do
    it "e1" $
      or [False,True,False]
    it "e2" $
      not (or [False,False,False])
    it "p1" $
      property prop_or
  describe "reverse" $ do
    it "e1" $
      E.reverse [4,2,5] `shouldBe`  [5,2,4]
    it "p1" $
      property prop_reverse
  describe "reverse.Acc" $
    it "e1" $
      reverseAcc [3,2] [7,5,1]  `shouldBe`  [1,5,7,3,2]
  describe "reverse'" $
    it "behaves correctly" $
      property prop_reverse_reverse'
  describe "filter" $ do
    it "e1" $
      E.filter even [4,5,2]  `shouldBe`  [4,2]
    it "e2" $
      E.filter odd  [4,5,2]  `shouldBe`  [5]
  describe "divisores" $
    it "e1" $
      divisores 24  `shouldBe`  [1,2,3,4,6,8,12,24]
  describe "esPrimo" $ do
    it "e1" $
      esPrimo 7
    it "e2" $
      not (esPrimo 9)
  describe "altura" $
    it "e1" $
      altura (Nodo (Hoja 3) (Nodo (Nodo (Hoja 1) (Hoja 7)) (Hoja 2)))
      `shouldBe` 3
  describe "mapArbol" $
    it "e1" $
      mapArbol (+ 1) (Nodo (Hoja 2) (Hoja 4))
      `shouldBe` Nodo (Hoja 3) (Hoja 5)
  describe "mismaForma" $ do
    it "e1" $
      mismaForma arbol3 (mapArbol (* 10) arbol3)
    it "e2" $
      not (mismaForma arbol1 arbol2)
    it "p1" $
      property prop_mismaForma_mismaForma'
  describe "creaArbol" $
    it "e1" $
      creaArbol 2
      `shouldBe` Nodo (Nodo (Hoja ()) (Hoja ())) (Nodo (Hoja ()) (Hoja ()))
  describe "injerta" $
    it "e1" $
      injerta (Nodo (Hoja (Hoja 'x')) (Hoja (Nodo (Hoja 'y') (Hoja 'z'))))
      `shouldBe` Nodo (Hoja 'x') (Nodo (Hoja 'y') (Hoja 'z'))
  describe "valor" $ do
    it "e1" $
      valor expr1  `shouldBe`  -8
    it "e2" $
      valor expr2  `shouldBe`  0
  describe "resta" $ do
    it "e1" $
      resta (Lit 42) (Lit 2)  `shouldBe`  Suma (Lit 42) (Op (Lit 2))
    it "p1" $
      property prop_resta
  describe "numeroOps" $ do
    it "e1" $
      numeroOps (Lit 3)                      `shouldBe`  0
    it "e2" $
      numeroOps (Suma (Lit 7) (Op (Lit 5)))  `shouldBe`  2
  describe "cadenaExpr" $
    it "e1" $
      cadenaExpr expr2 `shouldBe`
      "(if (- (3 + 5)) == 0 then 1 else 0)"
prop_conc :: [Int] -> [Int] -> Property
prop_conc xs ys = xs E.++ ys === xs P.++ ys

prop_or :: [Bool] -> Property
prop_or bs = or bs === P.or bs

prop_reverse :: [Int] -> Property
prop_reverse xs = reverse xs === P.reverse xs
