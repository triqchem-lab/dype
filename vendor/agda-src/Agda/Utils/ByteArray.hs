-- Stub: Parser doesn't need ByteArray operations.
{-# LANGUAGE MagicHash #-}
module Agda.Utils.ByteArray
  ( ByteArray#
  , byteArrayOnes#
  , byteArrayIsSubsetOf#
  , byteArrayDisjoint#
  , byteArrayFoldrBits#
  , byteArrayFoldlBits#
  , byteArrayFoldrBitsStrict#
  , byteArrayFoldlBitsStrict#
  ) where

import GHC.Exts (ByteArray#, Int#)

byteArrayOnes# :: Int# -> ByteArray#
byteArrayOnes# _ = error "ByteArray stub"

byteArrayIsSubsetOf# :: ByteArray# -> ByteArray# -> Int#
byteArrayIsSubsetOf# _ _ = error "ByteArray stub"

byteArrayDisjoint# :: ByteArray# -> ByteArray# -> Int#
byteArrayDisjoint# _ _ = error "ByteArray stub"

byteArrayFoldrBits# :: (Int -> a -> a) -> a -> ByteArray# -> a
byteArrayFoldrBits# _ a _ = a

byteArrayFoldlBits# :: (a -> Int -> a) -> a -> ByteArray# -> a
byteArrayFoldlBits# _ a _ = a

byteArrayFoldrBitsStrict# :: (Int -> a -> a) -> a -> ByteArray# -> a
byteArrayFoldrBitsStrict# _ a _ = a

byteArrayFoldlBitsStrict# :: (a -> Int -> a) -> a -> ByteArray# -> a
byteArrayFoldlBitsStrict# _ a _ = a
