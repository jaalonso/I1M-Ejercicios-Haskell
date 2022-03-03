{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Cadenas_de_bloques_Spec (main, spec) where

import Test.Hspec
import Cadenas_de_bloques

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "longitudCadena" $
    it "e1" $
      longitudCadena (BloqueOriginal |>2 |>5 |>2)  `shouldBe`  3
  describe "sumaCadena" $
    it "e1" $
      sumaCadena (BloqueOriginal |>2 |>5 |>2)  `shouldBe`  9
  describe "maxCadena" $
    it "e1" $
      maxCadena (BloqueOriginal |>2 |>5 |>2) `shouldBe`  5
  describe "cadenaMasLarga" $ do
    it "e1" $
      cadenaMasLarga (BloqueOriginal |>7) (BloqueOriginal |>2 |>1) `shouldBe`
      BloqueOriginal |>2 |>1
    it "e1" $
      cadenaMasLarga (BloqueOriginal |>2 |>1) (BloqueOriginal |>7) `shouldBe`
      BloqueOriginal |>2 |>1
    it "e1" $
      cadenaMasLarga (BloqueOriginal |>2) (BloqueOriginal |>7) `shouldBe`
      BloqueOriginal |>2
  describe "cadenaValida" $ do
    it "e1" $
      cadenaValida (BloqueOriginal |>3 |>6 |>7)
    it "e2" $
      not (cadenaValida (BloqueOriginal |>3 |>3 |>7))
    it "e3" $
      not (cadenaValida (BloqueOriginal |>3 |>2 |>7))
  describe "esPrefijoDe" $ do
    it "e1" $
      (BloqueOriginal |>1 |>3) `esPrefijoDe` (BloqueOriginal |>1 |>3 |>2)
    it "e2" $
      not ((BloqueOriginal |>1 |>3) `esPrefijoDe` (BloqueOriginal |>1 |>2 |>3))
    it "e3" $
      (BloqueOriginal |>1 |>3) `esPrefijoDe` (BloqueOriginal |>1 |>3)
  describe "sonCompatibles" $ do
    it "e1" $
      sonCompatibles (BloqueOriginal |>3) (BloqueOriginal |>3 |>2 |>1)
    it "e2" $
      sonCompatibles (BloqueOriginal |>3 |>2 |>1) (BloqueOriginal |>3)
    it "e3" $
      not (sonCompatibles (BloqueOriginal |>2 |>1) (BloqueOriginal |>3))
  describe "prefijoComun" $ do
    it "e1" $
      prefijoComun (BloqueOriginal |>3 |>2 |>5) (BloqueOriginal |>3 |>2 |>7)
      `shouldBe` BloqueOriginal |>3 |>2
    it "e2" $
       prefijoComun (BloqueOriginal |>3 |>5 |>7) (BloqueOriginal |>3 |>2 |>7)
      `shouldBe` BloqueOriginal |> 3
    it "e3" $
      prefijoComun (BloqueOriginal |>4 |>5 |>7) (BloqueOriginal |>3 |>2 |>7)
      `shouldBe` BloqueOriginal
  describe "tieneBloqueProp" $ do
    it "e1" $
      tieneBloqueProp even (BloqueOriginal |>3 |>2 |>5)
    it "e2" $
      not (tieneBloqueProp even (BloqueOriginal |>3 |>7 |>5))
  describe "tieneBloque" $ do
    it "e1" $
      tieneBloque 7 (BloqueOriginal |>3 |>7 |>5)
    it "e2" $
      not (tieneBloque 8 (BloqueOriginal |>3 |>7 |>5))
  describe "bloquesUnicos" $ do
    it "e1" $
      bloquesUnicos (BloqueOriginal |>3 |>7 |>5)
    it "e2" $
      not (bloquesUnicos (BloqueOriginal |>3 |>7 |>3))
  describe "todosBloquesProp" $ do
    it "e1" $
      todosBloquesProp (== 'x') BloqueOriginal
    it "e2" $
      todosBloquesProp even (BloqueOriginal |> 2 |> 4)
    it "e3" $
      not (todosBloquesProp even (BloqueOriginal |> 2 |> 3))
  describe "maxCadenas" $ do
    it "e1" $
      let c1 = BloqueOriginal |>3
          c2 = BloqueOriginal |>5 |>1
          c3 = BloqueOriginal |>2 |>1 |>2
      in maxCadenas [c1, c2, c3] `shouldBe` 3
    it "e2" $
      let d1 = BloqueOriginal |>3
          d2 = BloqueOriginal |>5 |>1
          d3 = BloqueOriginal |>2 |>1 |>2
      in maxCadenas' [d1, d2, d3] `shouldBe` 3
  describe "mayorPrefijoComun" $ do
    it "e1" $
      let c1 = BloqueOriginal |>3 |>5 |>7 |>4
          c2 = BloqueOriginal |>3 |>5 |>2
      in mayorPrefijoComun [c1, c2] `shouldBe` BloqueOriginal |>3 |>5
    it "e2" $
      let c1 = BloqueOriginal |>3 |>5 |>7 |>4
          c2 = BloqueOriginal |>3 |>5 |>2
          c3 = BloqueOriginal |>5 |>2
      in mayorPrefijoComun [c1, c2, c3] `shouldBe` BloqueOriginal
  describe "balancesCadena" $
    it "e1" $
      balancesCadena (BloqueOriginal |>2 |>8 |>4)
      `shouldBe` BloqueOriginal |>2 |>10 |>14
  describe "cadenaSinSaldosNegativos" $ do
    it "e1" $
      cadenaSinSaldosNegativos (BloqueOriginal |>2 |>8 |>4)
    it "e2" $
      cadenaSinSaldosNegativos (BloqueOriginal |>2 |>(-1) |>4)
    it "e3" $
      not (cadenaSinSaldosNegativos (BloqueOriginal |>2 |>(-3) |>4))
  describe "acortaMientras" $ do
    it "e1" $
      acortaMientras even (BloqueOriginal |>2 |>3 |>4 |>6)
      `shouldBe` Bloque (Bloque BloqueOriginal 2) 3
    it "e2" $
      acortaMientras even (BloqueOriginal |>2 |>8 |>4 |>6)
      `shouldBe` BloqueOriginal
    it "e3" $
      acortaMientras even (BloqueOriginal |>2 |>8 |>4 |>5)
      `shouldBe` Bloque (Bloque (Bloque (Bloque BloqueOriginal 2) 8) 4) 5
  describe "construyeCadena" $ do
    it "e1" $
      construyeCadena (-5) `shouldBe` BloqueOriginal
    it "e2" $
      construyeCadena 3 `shouldBe` BloqueOriginal |> 1 |> 2 |> 3
  describe "replicaCadena" $ do
    it "e1" $
      replicaCadena (-7) 5 `shouldBe` BloqueOriginal
    it "e2" $
      replicaCadena 3 5 `shouldBe` BloqueOriginal |> 5 |> 5 |> 5
  describe "prefijo" $ do
    it "e1" $
      prefijo 2 (BloqueOriginal |> 3 |> 7 |> 5 |> 4)
      `shouldBe` Bloque (Bloque BloqueOriginal 3) 7
    it "e2" $
      prefijo 5 (BloqueOriginal |> 3 |> 7 |> 5 |> 4)
      `shouldBe` Bloque (Bloque (Bloque (Bloque BloqueOriginal 3) 7) 5) 4
    it "e3" $
      prefijo (-3) (BloqueOriginal |> 3 |> 7 |> 5 |> 4)
      `shouldBe` BloqueOriginal
