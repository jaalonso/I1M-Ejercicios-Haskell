{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Combinatoria_Spec (main, spec) where

import Combinatoria
import Test.QuickCheck
import Data.List (genericLength)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "subconjunto" $ do
    it "e1" $
      subconjunto [1,3,2,3] [1,2,3]  `shouldBe`  True
    it "e2" $
      subconjunto [1,3,4,3] [1,2,3]  `shouldBe`  False
    it "e3" $
      subconjunto2 [1,3,2,3] [1,2,3]  `shouldBe`  True
    it "e4" $
      subconjunto2 [1,3,4,3] [1,2,3]  `shouldBe`  False
    it "e5" $
      subconjunto3 [1,3,2,3] [1,2,3]  `shouldBe`  True
    it "e6" $
      subconjunto3 [1,3,4,3] [1,2,3]  `shouldBe`  False
    it "e7" $
      property prop_equiv_subconjunto
    it "e8" $
      subconjunto' [1,3,2,3] [1,2,3]  `shouldBe`  True
    it "e9" $
      subconjunto' [1,3,4,3] [1,2,3]  `shouldBe`  False
    it "e10" $
      property prop_equivalencia

  describe "igualConjunto" $ do
    it "e1" $
      igualConjunto [1..10] [10,9..1]   `shouldBe`  True
    it "e2" $
      igualConjunto [1..10] [11,10..1]  `shouldBe`  False

  describe "subconjuntos" $ do
    it "e1" $
      subconjuntos [2,3,4]
      `shouldBe` [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
    it "e2" $
      subconjuntos [1,2,3,4]
      `shouldBe` [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],[1],
                    [2,3,4],  [2,3],  [2,4],  [2],  [3,4],  [3],  [4], []]
    it "e3" $
      subconjuntos' [2,3,4]
      `shouldBe` [[],[4],[3],[3,4],[2],[2,4],[2,3],[2,3,4]]
    it "e4" $
      subconjuntos' [1,2,3,4]
      `shouldBe` [[],[4],[3],[3,4],[2],[2,4],[2,3],[2,3,4],
                  [1],[1,4],[1,3],[1,3,4],[1,2],[1,2,4],[1,2,3],[1,2,3,4]]

  describe "intercala1" $ do
    it "e1" $
      intercala1 1 [2,3]  `shouldBe`  [[1,2,3],[2,1,3],[2,3,1]]
    it "e2" $
      intercala 1 [2,3]  `shouldBe`  [[1,2,3],[2,1,3],[2,3,1]]

  describe "permutaciones" $ do
    it "e1" $
      permutaciones "bc"   `shouldBe`  ["bc","cb"]
    it "e2" $
      permutaciones "abc"  `shouldBe`  ["abc","bac","bca","acb","cab","cba"]
    it "e3" $
      permutaciones2 "bc"   `shouldBe`  ["bc","cb"]
    it "e4" $
      permutaciones2 "abc"  `shouldBe`  ["abc","bac","bca","acb","cab","cba"]
    it "e5" $
      permutaciones3 "bc"   `shouldBe`  ["bc","cb"]
    it "e6" $
      permutaciones3 "abc"  `shouldBe`  ["abc","bac","bca","acb","cab","cba"]

  describe "permutacionesN" $ do
    it "e1" $
      permutacionesN 3
      `shouldBe` [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
    it "e2" $
      permutacionesN2 3
      `shouldBe` [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

  describe "numeroPermutacionesN" $ do
    it "e1" $
      numeroPermutacionesN 3  `shouldBe`  6
    it "e2" $
      numeroPermutacionesN 4  `shouldBe`  24

  describe "fact" $
    it "e1" $
      fact 3  `shouldBe`  6

  describe "numeroPermutacionesN'" $ do
    it "e1" $
      numeroPermutacionesN' 3  `shouldBe`  6
    it "e2" $
      numeroPermutacionesN' 4  `shouldBe`  24

  describe "prop_numeroPermutacionesN" $
    it "e1" $
      prop_numeroPermutacionesN 5  `shouldBe`  True

  describe "combinaciones" $ do
    it "e1" $
      combinaciones 2 "bcde"
      `shouldBe` ["bc","bd","be","cd","ce","de"]
    it "e2" $
      combinaciones 3 "bcde"
      `shouldBe` ["bcd","bce","bde","cde"]
    it "e3" $
      combinaciones 3 "abcde"
      `shouldBe` ["abc","abd","abe","acd","ace","ade","bcd","bce","bde","cde"]
    it "e4" $
      combinaciones2 2 "bcde"
      `shouldBe` ["bc","bd","be","cd","ce","de"]
    it "e5" $
      combinaciones2 3 "bcde"
      `shouldBe` ["bcd","bce","bde","cde"]
    it "e6" $
      combinaciones2 3 "abcde"
      `shouldBe` ["abc","abd","abe","acd","ace","ade","bcd","bce","bde","cde"]
    it "e7" $
      combinaciones3 2 "bcde"
      `shouldBe` ["bc","bd","be","cd","ce","de"]
    it "e8" $
      combinaciones3 3 "bcde"
      `shouldBe` ["bcd","bce","bde","cde"]
    it "e9" $
      combinaciones3 3 "abcde"
      `shouldBe` ["abc","abd","abe","acd","ace","ade","bcd","bce","bde","cde"]

  describe "combinacionesN" $ do
    it "e1" $
      combinacionesN 4 2
      `shouldBe` [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
    it "e2" $
      combinacionesN 4 3
      `shouldBe` [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
    it "e3" $
      combinacionesN2 4 2
      `shouldBe` [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
    it "e4" $
      combinacionesN2 4 3
      `shouldBe` [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]

  describe "numeroCombinaciones" $ do
    it "e1" $
      numeroCombinaciones 4 2  `shouldBe`  6
    it "e2" $
      numeroCombinaciones 4 3  `shouldBe`  4
    it "e3" $
      numeroCombinaciones2 4 2  `shouldBe`  6
    it "e4" $
      numeroCombinaciones2 4 3  `shouldBe`  4
    it "e5" $
      numeroCombinaciones3 4 2  `shouldBe`  6
    it "e6" $
      numeroCombinaciones3 4 3  `shouldBe`  4

  describe "comb" $ do
    it "e1" $
      comb 4 2  `shouldBe`  6
    it "e2" $
      comb 4 3  `shouldBe`  4

  describe "numeroCombinaciones'" $ do
    it "e1" $
      numeroCombinaciones' 4 2  `shouldBe`  6
    it "e2" $
      numeroCombinaciones' 4 3  `shouldBe`  4

  describe "prop_numeroCombinaciones" $
    it "e1" $
      prop_numeroCombinaciones 5  `shouldBe`  True

  describe "combinacionesR" $ do
    it "e1" $
      combinacionesR 2 "abc"
      `shouldBe` ["aa","ab","ac","bb","bc","cc"]
    it "e2" $
      combinacionesR 3 "bc"
      `shouldBe` ["bbb","bbc","bcc","ccc"]
    it "e3" $
      combinacionesR 3 "abc"
      `shouldBe` ["aaa","aab","aac","abb","abc","acc","bbb","bbc","bcc","ccc"]

  describe "combinacionesRN" $ do
    it "e1" $
      combinacionesRN 3 2
      `shouldBe` [[1,1],[1,2],[1,3],[2,2],[2,3],[3,3]]
    it "e2" $
      combinacionesRN 2 3
      `shouldBe` [[1,1,1],[1,1,2],[1,2,2],[2,2,2]]
    it "e3" $
      combinacionesRN2 3 2
      `shouldBe` [[1,1],[1,2],[1,3],[2,2],[2,3],[3,3]]
    it "e4" $
      combinacionesRN2 2 3
      `shouldBe` [[1,1,1],[1,1,2],[1,2,2],[2,2,2]]

  describe "numeroCombinacionesR" $ do
    it "e1" $
      numeroCombinacionesR 3 2  `shouldBe`  6
    it "e2" $
      numeroCombinacionesR 2 3  `shouldBe`  4
    it "e3" $
      numeroCombinacionesR' 3 2  `shouldBe`  6
    it "e4" $
      numeroCombinacionesR' 2 3  `shouldBe`  4

  describe "prop_numeroCombinacionesR" $
    it "e1" $
      prop_numeroCombinacionesR 5  `shouldBe`  True

  describe "variaciones" $
    it "e1" $
      variaciones 2 "abc"  `shouldBe`  ["ab","ba","ac","ca","bc","cb"]

  describe "variacionesN" $
    it "e1" $
      variacionesN 3 2  `shouldBe`  [[1,2],[2,1],[1,3],[3,1],[2,3],[3,2]]

  describe "numeroVariaciones" $ do
    it "e1" $
      numeroVariaciones 4 2  `shouldBe`  12
    it "e2" $
      numeroVariaciones 4 3  `shouldBe`  24
    it "e3" $
      numeroVariaciones2 4 2  `shouldBe`  12
    it "e4" $
      numeroVariaciones2 4 3  `shouldBe`  24
    it "e5" $
      numeroVariaciones' 4 2  `shouldBe`  12
    it "e6" $
      numeroVariaciones' 4 3  `shouldBe`  24

  describe "prop_numeroVariaciones" $
    it "e1" $
      prop_numeroVariaciones 5  `shouldBe`  True

  describe "variacionesR" $ do
    it "e1" $
      variacionesR 1 "ab"
      `shouldBe` ["a","b"]
    it "e2" $
      variacionesR 2 "ab"
      `shouldBe` ["aa","ab","ba","bb"]
    it "e3" $
      variacionesR 3 "ab"
      `shouldBe` ["aaa","aab","aba","abb","baa","bab","bba","bbb"]

  describe "variacionesRN" $ do
    it "e1" $
      variacionesRN 3 2
      `shouldBe` [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
    it "e2" $
      variacionesRN 2 3
      `shouldBe` [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]]

  describe "numeroVariacionesR" $ do
    it "e1" $
      numeroVariacionesR 3 2  `shouldBe`  9
    it "e2" $
      numeroVariacionesR 2 3  `shouldBe`  8
    it "e2" $
      numeroVariacionesR' 3 2  `shouldBe`  9
    it "e2" $
      numeroVariacionesR' 2 3  `shouldBe`  8

  describe "prop_numeroVariacionesR" $
    it "e1" $
      prop_numeroVariacionesR 5  `shouldBe`  True
