module Combinatoria_con_librerias_Spec (main, spec) where

import Combinatoria_con_librerias
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "subconjuntos" $ do
    it "e1" $
      subconjuntos [2,3,4]
      `shouldBe` [[],[4],[3],[3,4],[2],[2,4],[2,3],[2,3,4]]
    it "e2" $
      subconjuntos [1,2,3,4]
      `shouldBe` [[],
                  [4],
                  [3],[3,4],
                  [2],[2,4],[2,3],[2,3,4],
                  [1],[1,4],[1,3],[1,3,4],[1,2],[1,2,4],[1,2,3],[1,2,3,4]]

  describe "permutaciones" $ do
    it "e1" $
      permutaciones "bc"   `shouldBe`  ["bc","cb"]
    it "e2" $
      permutaciones "abc"  `shouldBe`  ["abc","bac","cba","bca","cab","acb"]

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

  describe "numeroCombinaciones" $ do
    it "e1" $
      numeroCombinaciones 4 2  `shouldBe`  6
    it "e2" $
      numeroCombinaciones 4 3  `shouldBe`  4

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

  describe "variaciones" $
    it "e1" $
      variaciones 2 "abc"  `shouldBe`  ["ab","ba","ac","ca","bc","cb"]

  describe "variacionesR" $ do
    it "e1" $
      variacionesR 1 "ab"
      `shouldBe` ["a","b"]
    it "e2" $
      variacionesR 2 "ab"
      `shouldBe` ["aa","ba","ab","bb"]
    it "e3" $
      variacionesR 3 "ab"
      `shouldBe` ["aaa","baa","aba","bba","aab","bab","abb","bbb"]
