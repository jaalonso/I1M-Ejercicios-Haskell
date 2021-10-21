module Tipos_de_datos_algebraicos_Spec (main, spec) where

import Tipos_de_datos_algebraicos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sumaArbol" $
    it "e1" $
      sumaArbol (N1 2 (N1 5 (N1 3 H1 H1) (N1 7 H1 H1)) (N1 4 H1 H1))
      `shouldBe` 21

  describe "mapArbol" $
    it "e1" $
      mapArbol (+1) (N1 2 (N1 5 (N1 3 H1 H1) (N1 7 H1 H1)) (N1 4 H1 H1))
      `shouldBe` N1 3 (N1 6 (N1 4 H1 H1) (N1 8 H1 H1)) (N1 5 H1 H1)

  describe "ramaIzquierda" $
    it "e1" $
      ramaIzquierda (N1 2 (N1 5 (N1 3 H1 H1) (N1 7 H1 H1)) (N1 4 H1 H1))
      `shouldBe` [2,5,3]

  describe "balanceado" $ do
    it "e1" $
      balanceado (N1 5 H1 (N1 3 H1 H1))           `shouldBe` True
    it "e2" $
      balanceado (N1 5 H1 (N1 3 (N1 4 H1 H1) H1)) `shouldBe` False

  describe "igualBorde" $ do
    it "e1" $
      igualBorde arbol1 arbol2  `shouldBe`  True
    it "e2" $
      igualBorde arbol1 arbol3  `shouldBe`  False
    it "e3" $
      igualBorde arbol1 arbol4  `shouldBe`  False

  describe "igualEstructura" $ do
    it "e1" $
      igualEstructura ej3arbol1 ej3arbol2 `shouldBe` True
    it "e2" $
      igualEstructura ej3arbol1 ej3arbol3 `shouldBe` False
    it "e3" $
      igualEstructura ej3arbol1 ej3arbol4 `shouldBe` False

  describe "algunoArbol" $ do
    it "e1" $
      algunoArbol (N3 5 (N3 3 (H3 1) (H3 4)) (H3 2)) (>4)  `shouldBe`  True
    it "e2" $
      algunoArbol (N3 5 (N3 3 (H3 1) (H3 4)) (H3 2)) (>7)  `shouldBe`  False

  describe "nivel" $ do
    it "e1" $
      nivel 0 (N3 7 (N3 2 (H3 5) (H3 4)) (H3 9))  `shouldBe`  [7]
    it "e2" $
      nivel 1 (N3 7 (N3 2 (H3 5) (H3 4)) (H3 9))  `shouldBe`  [2,9]
    it "e3" $
      nivel 2 (N3 7 (N3 2 (H3 5) (H3 4)) (H3 9))  `shouldBe`  [5,4]
    it "e4" $
      nivel 3 (N3 7 (N3 2 (H3 5) (H3 4)) (H3 9))  `shouldBe`  []

  describe "arbolFactorizacion" $ do
    it "e1" $
      arbolFactorizacion 60 `shouldBe`
      N3 60 (N3 6 (H3 2) (H3 3)) (N3 10 (H3 2) (H3 5))
    it "e2" $
      arbolFactorizacion 45 `shouldBe` N3 45 (H3 5) (N3 9 (H3 3) (H3 3))
    it "e3" $
      arbolFactorizacion 7  `shouldBe` H3 7
    it "e4" $
      arbolFactorizacion 14 `shouldBe` N3 14 (H3 2) (H3 7)
    it "e5" $
      arbolFactorizacion 28 `shouldBe` N3 28 (N3 4 (H3 2) (H3 2)) (H3 7)
    it "e6" $
      arbolFactorizacion 84 `shouldBe`
      N3 84 (H3 7) (N3 12 (H3 3) (N3 4 (H3 2) (H3 2)))

  describe "arbolFactorizacion2" $ do
    it "e1" $
      arbolFactorizacion2 60 `shouldBe`
      N3 60 (N3 6 (H3 2) (H3 3)) (N3 10 (H3 2) (H3 5))
    it "e2" $
      arbolFactorizacion2 45 `shouldBe` N3 45 (H3 5) (N3 9 (H3 3) (H3 3))
    it "e3" $
      arbolFactorizacion2 7  `shouldBe` H3 7
    it "e4" $
      arbolFactorizacion2 14 `shouldBe` N3 14 (H3 2) (H3 7)
    it "e5" $
      arbolFactorizacion2 28 `shouldBe` N3 28 (N3 4 (H3 2) (H3 2)) (H3 7)
    it "e6" $
      arbolFactorizacion2 84 `shouldBe`
      N3 84 (H3 7) (N3 12 (H3 3) (N3 4 (H3 2) (H3 2)))

  describe "valorB" $ do
    it "e1" $
      valorB ej1 `shouldBe` True
    it "e2" $
      valorB ej2 `shouldBe` False

  describe "ramifica" $ do
    it "e1" $
      ramifica ejG1 (N 8 []) (>4)
      `shouldBe` N 1 [N 2 [],N 3 [N 4 []]]
    it "e2" $
      ramifica ejG1 (N 8 []) (>3)
      `shouldBe` N 1 [N 2 [],N 3 [N 4 [N 8 []]]]
    it "e3" $
      ramifica ejG1 (N 8 []) (>2)
      `shouldBe` N 1 [N 2 [],N 3 [N 4 [N 8 []],N 8 []]]
    it "e4" $
      ramifica ejG1 (N 8 []) (>1)
      `shouldBe` N 1 [N 2 [N 8 []],N 3 [N 4 [N 8 []],N 8 []]]
    it "e5" $
      ramifica ejG1 (N 8 []) (>0)
      `shouldBe` N 1 [N 2 [N 8 []],N 3 [N 4 [N 8 []],N 8 []],N 8 []]

  describe "valor" $
    it "e1" $
      valor (P1 (C1 2) (S1 (C1 3) (C1 7)))  `shouldBe`  20

  describe "aplica" $ do
    it "e1" $
      aplica (+2) (S1 (P1 (C1 3) (C1 5)) (P1 (C1 6) (C1 7)))
      `shouldBe` S1 (P1 (C1 5) (C1 7)) (P1 (C1 8) (C1 9))
    it "e2" $
      aplica (*2) (S1 (P1 (C1 3) (C1 5)) (P1 (C1 6) (C1 7)))
      `shouldBe` S1 (P1 (C1 6) (C1 10)) (P1 (C1 12) (C1 14))

  describe "valorE" $
    it "e1" $
      valorE (P2 X (S2 (C2 13) X)) 2  `shouldBe`  30

  describe "numVars" $ do
    it "e1" $
      numVars (C2 3)                 `shouldBe`  0
    it "e2" $
      numVars X                      `shouldBe`  1
    it "e3" $
      numVars (P2 X (S2 (C2 13) X))  `shouldBe`  2

  describe "valor3" $
    it "e1" $
      valor3 (P3 (C3 2) (S3 (V3 'a') (V3 'b'))) [('a',2),('b',5)]
      `shouldBe` 14

  describe "sumas" $ do
    it "e1" $
      sumas (P3 (V3 'z') (S3 (C3 3) (V3 'x')))  `shouldBe`  1
    it "e2" $
      sumas (S3 (V3 'z') (S3 (C3 3) (V3 'x')))  `shouldBe`  2
    it "e3" $
      sumas (P3 (V3 'z') (P3 (C3 3) (V3 'x')))  `shouldBe`  0

  describe "sustitucion" $ do
    it "e1" $
      sustitucion (P3 (V3 'z') (S3 (C3 3) (V3 'x'))) [('x',7),('z',9)]
      `shouldBe` P3 (C3 9) (S3 (C3 3) (C3 7))
    it "e2" $
      sustitucion (P3 (V3 'z') (S3 (C3 3) (V3 'y'))) [('x',7),('z',9)]
      `shouldBe` P3 (C3 9) (S3 (C3 3) (V3 'y'))

  describe "reducible " $ do
    it "e1" $
      reducible (S3 (C3 3) (C3 4))               `shouldBe` True
    it "e2" $
      reducible (S3 (C3 3) (V3 'x'))             `shouldBe` False
    it "e3" $
      reducible (S3 (C3 3) (P3 (C3 4) (C3 5)))   `shouldBe` True
    it "e4" $
      reducible (S3 (V3 'x') (P3 (C3 4) (C3 5))) `shouldBe` True
    it "e5" $
      reducible (S3 (C3 3) (P3 (V3 'x') (C3 5))) `shouldBe` False
    it "e6" $
      reducible (C3 3)                           `shouldBe` False
    it "e7" $
      reducible (V3 'x')                         `shouldBe` False

  describe "maximo" $
    it "e1" $
      maximo (E4 (S4 (C4 10) (P4 (R4 (C4 1) Y) Y)) 2) [-3..3]
      `shouldBe` (100,[0,1])

  describe "valorEG" $ do
    it "e1" $
      valorEG (A Su (A Re (C5 7) (C5 3)) (A Mu (C5 2) (C5 5)))  `shouldBe`  14
    it "e2" $
      valorEG (A Mu (A Re (C5 7) (C5 3)) (A Su (C5 2) (C5 5)))  `shouldBe`  28

  describe "valorEG2" $ do
    it "e1" $
      valorEG2 (A Su (A Re (C5 7) (C5 3)) (A Mu (C5 2) (C5 5)))  `shouldBe`  14
    it "e2" $
      valorEG2 (A Mu (A Re (C5 7) (C5 3)) (A Su (C5 2) (C5 5)))  `shouldBe`  28

  describe "valorEV" $ do
    it "e1" $
      valorEV (Vec 1 2)                                  `shouldBe`  (1,2)
    it "e2" $
      valorEV (Sum (Vec 1 2 ) (Vec 3 4))                 `shouldBe`  (4,6)
    it "e3" $
      valorEV (Mul 2 (Vec 3 4))                          `shouldBe`  (6,8)
    it "e4" $
      valorEV (Mul 2 (Sum (Vec 1 2 ) (Vec 3 4)))         `shouldBe`  (8,12)
    it "e5" $
      valorEV (Sum (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4)))  `shouldBe`  (8,12)

  describe "valorEV2" $ do
    it "e1" $
      valorEV2 (Vec 1 2)                                  `shouldBe`  (1,2)
    it "e2" $
      valorEV2 (Sum (Vec 1 2 ) (Vec 3 4))                 `shouldBe`  (4,6)
    it "e3" $
      valorEV2 (Mul 2 (Vec 3 4))                          `shouldBe`  (6,8)
    it "e4" $
      valorEV2 (Mul 2 (Sum (Vec 1 2 ) (Vec 3 4)))         `shouldBe`  (8,12)
    it "e5" $
      valorEV2 (Sum (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4)))  `shouldBe`  (8,12)
