module Problemas_basicos_de_grafos_Spec (main, spec) where

import Problemas_basicos_de_grafos
import Test.Hspec
import I1M.Grafo
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "completo" $
    it "e1" $
      show (completo 4)
      `shouldBe` "G ND (array (1,4) [(1,[(2,0),(3,0),(4,0)]),(2,[(1,0),(3,0),(4,0)]),(3,[(1,0),(2,0),(4,0)]),(4,[(1,0),(2,0),(3,0)])])"

  describe "grafoCiclo" $
    it "e1" $
      show (grafoCiclo 3)
      `shouldBe` "G ND (array (1,3) [(1,[(3,0),(2,0)]),(2,[(1,0),(3,0)]),(3,[(2,0),(1,0)])])"

  describe "nVertices" $ do
    it "e1" $
      nVertices (completo 4)  `shouldBe`  4
    it "e2" $
      nVertices (completo 5)  `shouldBe`  5

  describe "noDirigido" $ do
    it "e1" $
      noDirigido g1            `shouldBe`  True
    it "e2" $
      noDirigido g2            `shouldBe`  False
    it "e3" $
      noDirigido (completo 4)  `shouldBe`  True

  describe "incidentes" $ do
    it "e1" $
      incidentes g2 5  `shouldBe`  [1,2,4]
    it "e2" $
      adyacentes g2 5  `shouldBe`  []
    it "e3" $
      incidentes g1 5  `shouldBe`  [1,2,3,4]
    it "e4" $
      adyacentes g1 5  `shouldBe`  [1,2,3,4]

  describe "contiguos" $ do
    it "e1" $
      contiguos g2 5  `shouldBe`  [1,2,4]
    it "e2" $
      contiguos g1 5  `shouldBe`  [1,2,3,4]

  describe "lazos" $ do
    it "e1" $
      lazos g3 `shouldBe` [(2,2)]
    it "e2" $
      lazos g2 `shouldBe` []

  describe "nLazos" $ do
    it "e1" $
      nLazos g3  `shouldBe`  1
    it "e2" $
      nLazos g2  `shouldBe`  0

  describe "nAristas" $ do
    it "e1" $
      nAristas g1            `shouldBe`  8
    it "e2" $
      nAristas g2            `shouldBe`  7
    it "e3" $
      nAristas g10           `shouldBe`  4
    it "e4" $
      nAristas g12           `shouldBe`  3
    it "e5" $
      nAristas (completo 4)  `shouldBe`  6
    it "e5" $
      nAristas (completo 5)  `shouldBe`  10
    it "e6" $
      nAristas2 g1            `shouldBe`  8
    it "e7" $
      nAristas2 g2            `shouldBe`  7
    it "e8" $
      nAristas2 g10           `shouldBe`  4
    it "e9" $
      nAristas2 g12           `shouldBe`  3
    it "e10" $
      nAristas2 (completo 4)  `shouldBe`  6
    it "e11" $
      nAristas2 (completo 5)  `shouldBe`  10
    it "e12" $
      and [prop_nAristasCompleto n | n <- [1..20]] `shouldBe` True

  describe "gradoPos" $ do
    it "e1" $
      gradoPos g1 5  `shouldBe`  4
    it "e2" $
      gradoPos g2 5  `shouldBe`  0
    it "e3" $
      gradoPos g2 1  `shouldBe`  3
    it "e4" $
      gradoPos2 g1 5  `shouldBe`  4
    it "e5" $
      gradoPos2 g2 5  `shouldBe`  0
    it "e6" $
      gradoPos2 g2 1  `shouldBe`  3

  describe "gradoNeg" $ do
    it "e1" $
      gradoNeg g1 5  `shouldBe`  4
    it "e2" $
      gradoNeg g2 5  `shouldBe`  3
    it "e3" $
      gradoNeg g2 1  `shouldBe`  0

  describe "grado" $ do
    it "e1" $
      grado g1 5  `shouldBe`  4
    it "e2" $
      grado g2 5  `shouldBe`  3
    it "e3" $
      grado g2 1  `shouldBe`  3
    it "e4" $
      grado g3 2  `shouldBe`  4
    it "e5" $
      grado g3 1  `shouldBe`  2
    it "e6" $
      grado g3 3  `shouldBe`  2
    it "e7" $
      grado g5 1  `shouldBe`  2
    it "e8" $
      grado g10 3 `shouldBe`  4
    it "e9" $
      grado g11 3 `shouldBe`  4

  describe "propiedades" $ do
    it "e1" $
      property prop_sumaGrados
    it "e2" $
      property prop_apretonManos
    it "e3" $
      property prop_numNodosGradoImpar
    it "e4" $
      and [prop_GradoCompleto n | n <- [1..30]] `shouldBe` True

  describe "regular" $ do
    it "e1" $
      regular g1            `shouldBe`  False
    it "e2" $
      regular g2            `shouldBe`  False
    it "e3" $
      regular (completo 4)  `shouldBe`  True
    it "e4" $
      prop_CompletoRegular 1 30 `shouldBe` True

  describe "regularidad" $ do
    it "e1" $
      regularidad g1              `shouldBe`  Nothing
    it "e2" $
      regularidad (completo 4)    `shouldBe`  Just 3
    it "e3" $
      regularidad (completo 5)    `shouldBe`  Just 4
    it "e4" $
      regularidad (grafoCiclo 4)  `shouldBe`  Just 2
    it "e5" $
      regularidad (grafoCiclo 5)  `shouldBe`  Just 2
    it "e6" $
      and [prop_completoRegular n | n <- [1..20]]  `shouldBe` True
    it "e7" $
      and [prop_cicloRegular n | n <- [3..20]] `shouldBe` True
