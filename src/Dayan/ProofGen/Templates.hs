{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Dayan.ProofGen.Templates where
import Data.Word (Word16); import Data.Text (Text); import qualified Data.Text as T; import Dayan.ProofGen.AST

genCrtLookup :: Word16 -> (Word16, Word16) -> Decl
genCrtLookup n (p, t) =
  let name = "crt-" <> T.pack (show n)
      ty = TApp (TApp (TDef "_≡_") (apps (Def "lookupCrt") [Lit (LNat n)])) (apps (Def "_,_") [Lit (LNat p), Lit (LNat t)])
  in DDef name ty [Clause [] Refl]

genAllCrtLookups :: [(Word16, (Word16, Word16))] -> [Decl]
genAllCrtLookups = map (uncurry genCrtLookup)

genEncodeDecode :: Word16 -> Decl
genEncodeDecode n =
  let name = "encdec-" <> T.pack (show n)
      ty = TApp (TApp (TDef "_≡_") (apps (Def "encode") [apps (Def "decode") [Lit (LNat n)]])) (Lit (LNat n))
  in DDef name ty [Clause [] Refl]

genEncodeDecodeRange :: Word16 -> Word16 -> [Decl]
genEncodeDecodeRange lo hi = map genEncodeDecode [lo..hi]

genAlignmentProof :: Decl
genAlignmentProof =
  let ty = TApp (TApp (TDef "_≡_") (apps (Def "toroidal") [apps (Def "stepN") [Lit (LNat 6624), Def "huangzhong"]])) (Lit (LNat 0))
      body = Trans (apps (Def "stepN-adds") [Lit (LNat 6624), Def "huangzhong"]) (Trans (apps (Def "+-assoc") [Lit (LNat 0), Lit (LNat 6624), Lit (LNat 0)]) Refl)
  in DDef "align-6624" ty [Clause [] body]

genT6VerificationFile :: Text -> [(Word16, (Word16, Word16))] -> AgdaFile
genT6VerificationFile modName crtEntries = AgdaFile
  { fileOpts = "{-# OPTIONS --rewriting #-}", fileModule = modName
  , fileDecls = [DImport "Agda.Builtin.Nat", DImport "Agda.Builtin.Equality", DComment "CRT lookup proofs"] ++ genAllCrtLookups crtEntries ++ [DComment "alignment", genAlignmentProof] }

genTryteVerificationFile :: Text -> AgdaFile
genTryteVerificationFile modName = AgdaFile
  { fileOpts = "{-# OPTIONS --guardedness #-}", fileModule = modName
  , fileDecls = [DImport "Sovereign.Core.Tryte", DComment "encode∘decode roundtrip"] ++ genEncodeDecodeRange 0 99 }
