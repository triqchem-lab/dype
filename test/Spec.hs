module Main where

import Test.Hspec
import Dayan.Core.Trit

main :: IO ()
main = hspec $ do
  describe "Trit — GF(3) 三进制基本单元" $ do

    context "编解码" $ do
      it "toNat" $ do
        toNat N `shouldBe` 0
        toNat Z `shouldBe` 1
        toNat P `shouldBe` 2
      it "fromNat" $ do
        fromNat 0 `shouldBe` Just N
        fromNat 1 `shouldBe` Just Z
        fromNat 2 `shouldBe` Just P
        fromNat 3 `shouldBe` Nothing
        fromNat 255 `shouldBe` Nothing
      it "toInt" $ do
        toInt N `shouldBe` (-1)
        toInt Z `shouldBe` 0
        toInt P `shouldBe` 1
      it "fromInt" $ do
        fromInt (-1) `shouldBe` Just N
        fromInt 0    `shouldBe` Just Z
        fromInt 1    `shouldBe` Just P
        fromInt 2    `shouldBe` Nothing

    context "Bounded/Enum" $ do
      it "minBound/maxBound" $ do
        minBound `shouldBe` N
        maxBound `shouldBe` P
      it "succ/pred cycle" $ do
        succ N `shouldBe` Z
        succ Z `shouldBe` P
        pred Z `shouldBe` N
        pred P `shouldBe` Z

    context "模 3 加法" $ do
      it "add identity (N)" $
        mapM_ (\x -> add N x `shouldBe` x) [N, Z, P]
      it "add full table (9 cases)" $ do
        add N N `shouldBe` N
        add N Z `shouldBe` Z
        add N P `shouldBe` P
        add Z N `shouldBe` Z
        add Z Z `shouldBe` P
        add Z P `shouldBe` N
        add P N `shouldBe` P
        add P Z `shouldBe` N
        add P P `shouldBe` Z

    context "模 3 乘法" $ do
      it "mul identity (Z)" $
        mapM_ (\x -> mul Z x `shouldBe` x) [N, Z, P]
      it "mul zero (N)" $
        mapM_ (\x -> mul N x `shouldBe` N) [N, Z, P]
      it "mul P pattern" $ do
        mul P N `shouldBe` N
        mul P Z `shouldBe` P
        mul P P `shouldBe` Z

    context "模 3 取负" $ do
      it "neg" $ do
        neg N `shouldBe` N
        neg Z `shouldBe` P
        neg P `shouldBe` Z
      it "neg double = id" $
        mapM_ (\x -> neg (neg x) `shouldBe` x) [N, Z, P]

    context "驻波叠加" $ do
      it "superpose table" $ do
        superpose N N `shouldBe` N
        superpose N Z `shouldBe` N
        superpose N P `shouldBe` Z
        superpose Z N `shouldBe` N
        superpose Z Z `shouldBe` Z
        superpose Z P `shouldBe` P
        superpose P N `shouldBe` Z
        superpose P Z `shouldBe` P
        superpose P P `shouldBe` P
      it "isCanceled" $ do
        isCanceled N P `shouldBe` True
        isCanceled P N `shouldBe` True
        isCanceled N Z `shouldBe` False
        isCanceled P P `shouldBe` False

    context "谓词" $ do
      it "isAbsorb" $ do
        isAbsorb N `shouldBe` True
        isAbsorb Z `shouldBe` False
        isAbsorb P `shouldBe` False
      it "isBalance" $ do
        isBalance N `shouldBe` False
        isBalance Z `shouldBe` True
        isBalance P `shouldBe` False
      it "isExpress" $ do
        isExpress N `shouldBe` False
        isExpress Z `shouldBe` False
        isExpress P `shouldBe` True
