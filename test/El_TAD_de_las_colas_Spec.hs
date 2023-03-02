{-# OPTIONS_GHC -fno-warn-unused-matches
                -fno-warn-unused-imports
                -fno-warn-orphans
#-}

module El_TAD_de_las_colas_Spec (main, spec) where

import El_TAD_de_las_colas
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ultimoCola" $ do
    it "e1" $
      ultimoCola ejCola4 `shouldBe` 4
    it "e2" $
      ultimoCola ejCola5 `shouldBe` 15

  describe "longitudCola" $
    it "e1" $
      longitudCola ejCola2 `shouldBe` 6

  describe "todosVerifican" $ do
    it "e1" $
      todosVerifican (>0) ejCola1 `shouldBe` True
    it "e2" $
      todosVerifican (>0) ejCola4 `shouldBe` False

  describe "algunoVerifica" $ do
    it "e1" $
      algunoVerifica (<0) ejCola1 `shouldBe` False
    it "e2" $
      algunoVerifica (<0) ejCola4 `shouldBe` True

  describe "extiendeCola" $ do
    it "e1" $
      show (extiendeCola ejCola2 ejCola3)
      `shouldBe` "C [17,14,11,8,5,2,10,9,8,7,6,5,4,3]"

  describe "intercalaColas" $ do
    it "e1" $
      show (intercalaColas ejCola2 ejCola4)
      `shouldBe` "C [17,4,14,3,11,3,8,0,5,10,2,8,3,7,-1,4]"

  describe "agrupaColas" $ do
    it "e1" $
      show (agrupaColas [ejCola3,ejCola3,ejCola4])
      `shouldBe` "C [10,4,10,3,9,3,9,0,8,10,8,8,7,3,7,7,6,-1,6,4,5,5,4,4,3,3]"
    it "e2" $
      show (agrupaColas2 [ejCola3,ejCola3,ejCola4])
      `shouldBe` "C [10,4,10,3,9,3,9,0,8,10,8,8,7,3,7,7,6,-1,6,4,5,5,4,4,3,3]"

  describe "perteneceCola" $ do
    it "e1" $
      perteneceCola 7 ejCola1  `shouldBe` True
    it "e2" $
      perteneceCola 70 ejCola1 `shouldBe` False

  describe "contenidaCola" $ do
    it "e1" $
      contenidaCola ejCola2 ejCola1 `shouldBe` True
    it "e2" $
      contenidaCola ejCola1 ejCola2 `shouldBe` False

  describe "prefijoCola" $ do
    it "e1" $
      prefijoCola ejCola3 ejCola2 `shouldBe` False
    it "e2" $
      prefijoCola ejCola5 ejCola1 `shouldBe` True

  describe "subCola" $ do
    it "e1" $
      subCola ejCola2 ejCola1 `shouldBe` False
    it "e2" $
      subCola ejCola3 ejCola1 `shouldBe` True

  describe "ordenadaCola" $ do
    it "e1" $
      ordenadaCola ejCola6 `shouldBe` True
    it "e2" $
      ordenadaCola ejCola4 `shouldBe` False

  describe "lista2Cola" $ do
    it "e1" $
      show (lista2Cola [1..6]) `shouldBe` "C [1,2,3,4,5,6]"

  describe "cola2Lista" $ do
    it "e1" $
      cola2Lista ejCola2 `shouldBe` [17,14,11,8,5,2]
    it "e2" $
      property prop_cola2Lista
    it "e3" $
      property prop_lista2Cola

  describe "maxCola" $ do
    it "e1" $
      maxCola ejCola4 `shouldBe` 10
    it "e2" $
      property prop_maxCola
