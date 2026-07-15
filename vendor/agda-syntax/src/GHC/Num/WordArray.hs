{-# OPTIONS_GHC -Wno-dodgy-imports #-}
-- | Compatibility shim: GHC 9.14 moved GHC.Num.WordArray to GHC.Internal.Bignum.WordArray
module GHC.Num.WordArray (module GHC.Internal.Bignum.WordArray) where
import GHC.Internal.Bignum.WordArray
