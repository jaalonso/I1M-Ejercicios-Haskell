module Rompecabeza_del_triomino_mediante_divide_y_venceras_Spec (main, spec) where

import Rompecabeza_del_triomino_mediante_divide_y_venceras
import Data.Matrix
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tablero" $
    it "e1" $
      toLists (tablero 4 (3,4))
      `shouldBe` [[0,0,0,0],[0,0,0,0],[0,0,0,-1],[0,0,0,0]]

  describe "pbInicial" $
    it "e1" $
      let (a,b) = pbInicial 4 (4,4) in (a, toLists b)
      `shouldBe` (1,[[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,-1]])

  describe "ind" $ do
    it "e1" $
      ind (pbInicial 2 (1,2))  `shouldBe`  True
    it "e2" $
      ind (pbInicial 4 (1,2))  `shouldBe`  False

  describe "posicionHueco" $
    it "e1" $
      posicionHueco (tablero 8 (5,2))  `shouldBe`  (5,2)

  describe "cuadranteHueco" $ do
    it "e1" $
      cuadranteHueco (tablero 8 (4,4))  `shouldBe`  1
    it "e2" $
      cuadranteHueco (tablero 8 (5,2))  `shouldBe`  2
    it "e3" $
      cuadranteHueco (tablero 8 (3,6))  `shouldBe`  3
    it "e4" $
      cuadranteHueco (tablero 8 (6,6))  `shouldBe`  4

  describe "centralHueco" $ do
    it "e1" $
      centralHueco (tablero 8 (5,2))  `shouldBe`  (5,4)
    it "e2" $
      centralHueco (tablero 8 (4,4))  `shouldBe`  (4,4)
    it "e3" $
      centralHueco (tablero 8 (3,6))  `shouldBe`  (4,5)
    it "e4" $
      centralHueco (tablero 8 (6,6))  `shouldBe`  (5,5)

  describe "centralesSinHueco" $
    it "e1" $
      centralesSinHueco (tablero 8 (5,2))  `shouldBe`  [(4,4),(4,5),(5,5)]

  describe "actualiza" $
    it "e1" $
      toLists (actualiza (identity 3) [((1,2),4),((3,1),5)])
      `shouldBe` [[1,4,0],[0,1,0],[5,0,1]]

  describe "triominoCentral" $
    it "e1" $
      toLists (triominoCentral (7,tablero 4 (4,4)))
      `shouldBe` [[0,0,0,0],[0,7,7,0],[0,7,0,0],[0,0,0,-1]]

  describe "resuelve" $
    it "e1" $
      toLists (resuelve (5,tablero 2 (2,2)))
      `shouldBe` [[5,5],[5,-1]]

  describe "divide" $
    it "e1" $
      [(a,toLists b) | (a,b) <- divide (3,tablero 4 (4,4))]
      `shouldBe` [(4,[[0,0],[3,0]]),(5,[[0,0],[0,3]]),(6,[[0,3],[0,0]]),(7,[[0,0],[0,-1]])]

  describe "combina" $
    it "e1" $
      let inicial = (1,tablero 4 (4,4)) :: (Int,Matrix Int)
          [p1,p2,p3,p4] = divide inicial
          [s1,s2,s3,s4] = map resuelve [p1,p2,p3,p4]
      in toLists (combina inicial [s1,s2,s3,s4])
      `shouldBe` [[3,3,2,2],[3,1,1,2],[4,1,5,5],[4,4,5,-1]]

  describe "triomino" $ do
    it "e1" $
      toLists (triomino 4 (4,4))
      `shouldBe` [[3,3,2,2],[3,1,1,2],[4,1,5,5],[4,4,5,-1]]
    it "e2" $
      toLists (triomino 4 (2,3))
      `shouldBe` [[3,3,2,2],[3,1,-1,2],[4,1,1,5],[4,4,5,5]]
    it "e3" $
      toLists (triomino 16 (5,6)) `shouldBe`
      [[7,7,6,6,6,6,5,5,6,6,5,5,5,5,4,4],
       [7,5,5,6,6,4,4,5,6,4,4,5,5,3,3,4],
       [8,5,9,9,7,7,4,8,7,4,8,8,6,6,3,7],
       [8,8,9,3,3,7,8,8,7,7,8,2,2,6,7,7],
       [8,8,7,3,9,-1,8,8,7,7,6,6,2,8,7,7],
       [8,6,7,7,9,9,7,8,7,5,5,6,8,8,6,7],
       [9,6,6,10,10,7,7,11,8,8,5,9,9,6,6,10],
       [9,9,10,10,10,10,11,11,1,8,9,9,9,9,10,10],
       [8,8,7,7,7,7,6,1,1,9,8,8,8,8,7,7],
       [8,6,6,7,7,5,6,6,9,9,7,8,8,6,6,7],
       [9,6,10,10,8,5,5,9,10,7,7,11,9,9,6,10],
       [9,9,10,4,8,8,9,9,10,10,11,11,5,9,10,10],
       [9,9,8,4,4,10,9,9,10,10,9,5,5,11,10,10],
       [9,7,8,8,10,10,8,9,10,8,9,9,11,11,9,10],
       [10,7,7,11,11,8,8,12,11,8,8,12,12,9,9,13],
       [10,10,11,11,11,11,12,12,11,11,12,12,12,12,13,13]]
