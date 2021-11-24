module El_problema_de_las_fichas_mediante_busqueda_en_espacio_de_estado_Spec (main, spec) where

import El_problema_de_las_fichas_mediante_busqueda_en_espacio_de_estado
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "inicial" $ do
    it "e1" $
      inicial 2 3  `shouldBe`  [B,B,H,V,V,V]
    it "e2" $
      inicial 3 2  `shouldBe`  [B,B,B,H,V,V]

  describe "final" $ do
    it "e1" $
      final 2 3  `shouldBe`  [V,V,V,H,B,B]
    it "e2" $
      final 3 2  `shouldBe`  [V,V,H,B,B,B]

  describe "sucesoresE" $ do
    it "e1" $
      sucesoresE [V,B,H,V,V,B] `shouldBe`
      [[V,H,B,V,V,B],[H,B,V,V,V,B],[V,B,V,H,V,B],[V,B,V,V,H,B],
       [V,B,B,V,V,H]]
    it "e2" $
      sucesoresE [B,B,B,H,V,V,V] `shouldBe`
      [[B,B,H,B,V,V,V],[B,H,B,B,V,V,V],[H,B,B,B,V,V,V],
       [B,B,B,V,H,V,V],[B,B,B,V,V,H,V],[B,B,B,V,V,V,H]]

  describe "inicialN" $ do
    it "e1" $
      inicialN 2 3  `shouldBe`  N [[B,B,H,V,V,V]]
    it "e2" $
      inicialN 3 2  `shouldBe`  N [[B,B,B,H,V,V]]

  describe "esFinalN" $ do
    it "e1" $
      esFinalN 2 1 (N [[V,H,B,B],[V,B,B,H],[H,B,B,V],[B,B,H,V]]) `shouldBe`
      True
    it "e2" $
      esFinalN 2 1 (N [[V,B,B,H],[H,B,B,V],[B,B,H,V]]) `shouldBe`
      False

  describe "sucesoresN" $ do
    it "e1" $
      sucesoresN (N [[H,B,B,V],[B,B,H,V]]) `shouldBe`
      [N [[B,H,B,V],[H,B,B,V],[B,B,H,V]],
       N [[V,B,B,H],[H,B,B,V],[B,B,H,V]]]
    it "e2" $
      sucesoresN (N [[B,H,B,V],[H,B,B,V],[B,B,H,V]]) `shouldBe`
      [N [[B,V,B,H],[B,H,B,V],[H,B,B,V],[B,B,H,V]]]

  describe "solucionesEE" $
    it "e1" $
      zip [0..] (head (solucionesEE 2 2)) `shouldBe`
      [( 0,[B,B,H,V,V]),
       ( 1,[B,H,B,V,V]),
       ( 2,[H,B,B,V,V]),
       ( 3,[V,B,B,H,V]),
       ( 4,[V,B,H,B,V]),
       ( 5,[V,H,B,B,V]),
       ( 6,[H,V,B,B,V]),
       ( 7,[B,V,H,B,V]),
       ( 8,[B,H,V,B,V]),
       ( 9,[H,B,V,B,V]),
       (10,[B,B,V,H,V]),
       (11,[B,B,V,V,H]),
       (12,[B,H,V,V,B]),
       (13,[H,B,V,V,B]),
       (14,[V,B,H,V,B]),
       (15,[V,H,B,V,B]),
       (16,[H,V,B,V,B]),
       (17,[B,V,H,V,B]),
       (18,[B,V,V,H,B]),
       (19,[H,V,V,B,B]),
       (20,[V,H,V,B,B]),
       (21,[V,V,H,B,B])]

  describe "heuristicaE" $
    it "e1" $
      heuristicaE [B,V,B,H,V,V,B] `shouldBe` 5

  describe "heuristicaN" $ do
    it "e1" $
      heuristicaN (N [[H,B,B,V],[B,B,H,V]])            `shouldBe`  2
    it "e2" $
      heuristicaN (N [[V,B,B,H],[H,B,B,V],[B,B,H,V]])  `shouldBe`  0

  describe "solucionesPM" $
    it "e1" $
      zip [0..] (head (solucionesPM 2 2)) `shouldBe`
      [(0,[B,B,H,V,V]),
       (1,[B,H,B,V,V]),
       (2,[B,V,B,H,V]),
       (3,[H,V,B,B,V]),
       (4,[V,H,B,B,V]),
       (5,[V,V,B,B,H]),
       (6,[V,V,B,H,B]),
       (7,[V,V,H,B,B])]

  describe "solucionesEscalada" $
    it "e1" $
      zip [0..] (head (solucionesEscalada 2 2)) `shouldBe`
      [(0,[B,B,H,V,V]),
       (1,[B,H,B,V,V]),
       (2,[B,V,B,H,V]),
       (3,[H,V,B,B,V]),
       (4,[V,H,B,B,V]),
       (5,[V,V,B,B,H]),
       (6,[V,V,B,H,B]),
       (7,[V,V,H,B,B])]

  describe "solucionesAnchura" $
    it "e1" $
      zip [0..] (head (solucionesAnchura 2 2)) `shouldBe`
      [(0,[B,B,H,V,V]),
       (1,[B,B,V,V,H]),
       (2,[B,H,V,V,B]),
       (3,[B,V,V,H,B]),
       (4,[H,V,V,B,B]),
       (5,[V,V,H,B,B])]
