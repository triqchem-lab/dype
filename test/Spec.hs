module Main where

import Test.Hspec
import Data.Word (Word8, Word16)
import Dayan.Core.Trit
import Dayan.Core.Tryte (Tryte(..), unTryte, mkTryte, mkTryteSafe,
  minTryte, maxTryte, balanceTryte, tryteCardinality,
  encode, decode, decodeRaw, tritAt, setTrit,
  allTrytes, findTryte)
import qualified Dayan.Core.Tryte as Tryte
import Dayan.Core.Torus
import qualified Dayan.Core.Torus as Torus
import qualified Dayan.Core.Constants as C
import Dayan.Compute.CRT
import Dayan.Compute.ModArith

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
        Tryte.isValid (Tryte 0)   `shouldBe` True
        Tryte.isValid (Tryte 728) `shouldBe` True
        Tryte.isValid (Tryte 729) `shouldBe` False
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
        mapM_ (\t -> Tryte.isValid t `shouldBe` True) (take 50 allTrytes)
      it "findTryte" $ do
        findTryte (\t -> unTryte t == 364) `shouldBe` Just balanceTryte

  describe "Torus — 离散环面 T⁶ (144×46)" $ do

    context "常数" $ do
      it "polarWinding"   $ polarWinding `shouldBe` 144
      it "toroidalWinding" $ toroidalWinding `shouldBe` 46
      it "holographicCardinality" $ holographicCardinality `shouldBe` 6624
      it "gcd = 2" $ gcdPolarToroidal `shouldBe` 2

    context "构造" $ do
      it "mkTorusPoint valid" $ do
        mkTorusPoint 0 0    `shouldNotBe` Nothing
        mkTorusPoint 143 45 `shouldNotBe` Nothing
      it "mkTorusPoint invalid" $ do
        mkTorusPoint 144 0  `shouldBe` Nothing
        mkTorusPoint 0 46   `shouldBe` Nothing
      it "huangzhong" $ do
        polar huangzhong `shouldBe` 0
        toroidal huangzhong `shouldBe` 0
      it "isValid" $ do
        Torus.isValid (TorusPoint 0 0)     `shouldBe` True
        Torus.isValid (TorusPoint 143 45)  `shouldBe` True
        Torus.isValid (TorusPoint 144 0)   `shouldBe` False

    context "步进" $ do
      it "stepPolar wrap" $ stepPolar 143 `shouldBe` 0
      it "stepPolar normal" $ stepPolar 0 `shouldBe` 1
      it "stepToroidal wrap" $ stepToroidal 45 `shouldBe` 0
      it "step both" $ do
        let s1 = step huangzhong
        polar s1 `shouldBe` 1
        toroidal s1 `shouldBe` 1
      it "stepN 144" $ do
        let s144 = stepN 144 huangzhong
        polar s144 `shouldBe` 0   -- 回到极向起点
        toroidal s144 `shouldBe` 144 `mod` 46  -- = 6

    context "对齐" $ do
      it "huangzhong is aligned" $ isAligned huangzhong `shouldBe` True
      it "step 1 is not aligned" $ isAligned (step huangzhong) `shouldBe` False
      it "6624 steps returns to aligned" $ do
        let s6624 = stepN 6624 huangzhong
        polar s6624 `shouldBe` 0
        toroidal s6624 `shouldBe` 0
        isAligned s6624 `shouldBe` True

    context "枚举" $ do
      it "allPoints count" $ length allPoints `shouldBe` Torus.holographicCardinality
      it "trajectory from huangzhong starts at origin" $
        head (trajectory huangzhong) `shouldBe` huangzhong
      it "trajectory length = 6624" $
        length (trajectory huangzhong) `shouldBe` Torus.holographicCardinality

  describe "Constants — 大衍体系核心常数" $ do
    context "缠绕数" $ do
      it "polarWinding"    $ C.polarWinding `shouldBe` 144
      it "toroidalWinding" $ C.toroidalWinding `shouldBe` 46
      it "polarHalf"       $ C.polarHalf `shouldBe` 72
    context "格点常数" $ do
      it "t6Cardinality"            $ C.t6Cardinality `shouldBe` 729
      it "holographicCardinality"   $ C.holographicCardinality `shouldBe` 6624
    context "群论" $ do
      it "a4GroupOrder"    $ C.a4GroupOrder `shouldBe` 12
      it "c3GroupOrder"    $ C.c3GroupOrder `shouldBe` 3
      it "ihGroupOrder"    $ C.ihGroupOrder `shouldBe` 120
    context "算术" $ do
      it "sovereignLCM"    $ C.sovereignLCM `shouldBe` 11609505792
      it "factor3pow11"    $ C.factor3pow11 `shouldBe` 177147
      it "factor2pow16"    $ C.factor2pow16 `shouldBe` 65536
      it "holographicPi ≈ 3.13" $ C.holographicPi `shouldSatisfy` (\x -> x > 3.1 && x < 3.14)
    context "幻方" $ do
      it "magicSum4x4" $ C.magicSum4x4 `shouldBe` 34
      it "twelveTones" $ C.twelveTones `shouldBe` 12

  describe "CRT — 中国剩余定理查表" $ do
    context "查表" $ do
      it "lookupCrt 0"   $ lookupCrt 0   `shouldBe` (0, 0)
      it "lookupCrt 144" $ lookupCrt 144 `shouldBe` (0, 6 :: Word8)   -- 144%144=0, 144%46=6
      it "lookupCrt 46"  $ lookupCrt 46  `shouldBe` (46, 0 :: Word8)  -- 46%144=46, 46%46=0
      it "lookupCrt 6623" $ lookupCrt 6623 `shouldBe` (143, 45 :: Word8)
    context "投影" $ do
      it "length projectAll = 6624" $
        length projectAll `shouldBe` 6624
      it "all projections valid" $
        mapM_ (\(_, (p, t)) -> do
          p `shouldSatisfy` (< 144)
          t `shouldSatisfy` (< 46)
          ) (take 100 projectAll)
    context "双向一致性" $ do
      it "lookup then reconstruct = id (前100个)" $
        mapM_ (\i -> lookupIndex (lookupPolar i) (lookupToroidal i) `shouldBe` i) [0..99]
    context "gcd" $ do
      it "egcd(46,144) = 2" $ do
        let (g, _, _) = egcd 144 46
        g `shouldBe` 2

  describe "ModArith — 快速模算术" $ do
    context "mod 3" $ do
      it "mod3 0=0, 1=1, 2=2, 3=0" $ do
        mod3 0 `shouldBe` 0; mod3 1 `shouldBe` 1
        mod3 2 `shouldBe` 2; mod3 3 `shouldBe` 0
    context "unpack3/pack3" $ do
      it "roundtrip 0..99" $
        mapM_ (\n -> pack3 (unpack3 n) `shouldBe` n) [0..99 :: Word16]
      it "unpack3 729" $ unpack3 729 `shouldBe` replicate 6 0  -- wraps
    context "mod 12" $ do
      it "mod12" $ mod12 11 `shouldBe` 11
      it "isZhonglv 11" $ isZhonglv 11 `shouldBe` True
      it "isZhonglv 0"  $ isZhonglv 0  `shouldBe` False
    context "mod 46" $ do
      it "mod46 46=0" $ mod46 46 `shouldBe` 0
      it "isMultipleOf46 6624" $ isMultipleOf46 6624 `shouldBe` True

