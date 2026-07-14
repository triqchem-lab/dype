module Main where

import Test.Hspec
import Dayan.Core.Trit
import Dayan.Core.Tryte

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

  describe "Tryte — Fin 729 (6-trit 格点)" $ do

    context "构造与验证" $ do
      it "mkTryte" $ do
        unTryte (mkTryte 0) `shouldBe` 0
        unTryte (mkTryte 728) `shouldBe` 728
      it "mkTryteSafe" $ do
        mkTryteSafe 0   `shouldNotBe` Nothing
        mkTryteSafe 728 `shouldNotBe` Nothing
        mkTryteSafe 729 `shouldBe` Nothing
      it "isValid" $ do
        isValid (Tryte 0)   `shouldBe` True
        isValid (Tryte 728) `shouldBe` True
        isValid (Tryte 729) `shouldBe` False
      it "minTryte" $ unTryte minTryte `shouldBe` 0
      it "maxTryte" $ unTryte maxTryte `shouldBe` 728
      it "balanceTryte" $ unTryte balanceTryte `shouldBe` 364

    context "基3 编解码" $ do
      it "encode/decode roundtrip" $ do
        let test n = case encode (decode (Tryte n)) of
                       Just t  -> unTryte t `shouldBe` n
                       Nothing -> expectationFailure "encode failed"
        mapM_ test [0..100]
      it "decode/encode roundtrip" $ do
        let trits = [N, Z, P, N, Z, P]  -- v0=N, v1=Z, v2=P, v3=N, v4=Z, v5=P
        case encode trits of
          Just t  -> decode t `shouldBe` trits
          Nothing -> expectationFailure "encode failed"
      it "decodeRaw length" $ do
        length (decodeRaw (Tryte 0)) `shouldBe` 6
      it "specific encoding: all N" $ do
        case encode [N,N,N,N,N,N] of
          Just t  -> unTryte t `shouldBe` 0
          Nothing -> expectationFailure "encode all-N failed"
      it "specific encoding: all P" $ do
        case encode [P,P,P,P,P,P] of
          Just t  -> unTryte t `shouldBe` 728
          Nothing -> expectationFailure "encode all-P failed"
      it "specific encoding: [N,Z,P,N,Z,P]" $ do
        -- v0=N(0), v1=Z(1), v2=P(2), v3=N(0), v4=Z(1), v5=P(2)
        -- = 0 + 3*1 + 9*2 + 27*0 + 81*1 + 243*2
        -- = 0 + 3 + 18 + 0 + 81 + 486 = 588
        case encode [N,Z,P,N,Z,P] of
          Just t  -> unTryte t `shouldBe` 588
          Nothing -> expectationFailure "encode failed"

    context "Trit 级访问" $ do
      it "tritAt" $ do
        tritAt (Tryte 588) 0 `shouldBe` Just N   -- v0
        tritAt (Tryte 588) 1 `shouldBe` Just Z   -- v1
        tritAt (Tryte 588) 2 `shouldBe` Just P   -- v2
        tritAt (Tryte 588) 5 `shouldBe` Just P   -- v5
        tritAt (Tryte 588) (-1) `shouldBe` Nothing
        tritAt (Tryte 588) 6 `shouldBe` Nothing
      it "setTrit" $ do
        case setTrit (Tryte 0) 0 P of
          Just t  -> unTryte t `shouldBe` 2  -- 仅 v0 改为 P(2)
          Nothing -> expectationFailure "setTrit failed"

    context "枚举" $ do
      it "allTrytes count" $ length allTrytes `shouldBe` 729
      it "allTrytes are valid" $
        mapM_ (\t -> isValid t `shouldBe` True) (take 50 allTrytes)
      it "findTryte" $ do
        findTryte (\t -> unTryte t == 364) `shouldBe` Just balanceTryte
