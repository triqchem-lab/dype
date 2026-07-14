{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Dayan.ProofGen.AST where

import Data.Word (Word16)
import Data.Text (Text)

type QName = Text
type Name = Text
type ModuleName = Text

data Type = TSet | TPi Name Type Type | TApp Type Term | TDef QName | TNat | TFin Term | TVec Type Term | TFun Type Type deriving (Show,Eq)
data Term = Var Name | Def QName | App Term Term | Lam Name Term | Pi Name Type Term | Lit Lit | Refl | Sym Term | Trans Term Term | Cong Term Term | Subst Type Term Term Term | Ann Term Type | Hole deriving (Show,Eq)
data Lit = LNat Word16 | LZero | LSuc Term deriving (Show,Eq)
data Pattern = PVar Name | PCon QName [Pattern] | PLit Lit | PWild deriving (Show,Eq)
data Clause = Clause { clPats :: [Pattern], clBody :: Term } deriving (Show,Eq)
data Decl = DModule ModuleName [Decl] | DOpen ModuleName | DOpenUsing ModuleName [Name] | DImport ModuleName | DPostulate Name Type | DDef Name Type [Clause] | DData Name [Name] [ConDecl] | DComment Text deriving (Show,Eq)
data ConDecl = ConDecl { conName :: Name, conType :: Type } deriving (Show,Eq)
data AgdaFile = AgdaFile { fileOpts :: Text, fileModule :: ModuleName, fileDecls :: [Decl] } deriving (Show,Eq)

apps :: Term -> [Term] -> Term; apps = foldl App
lams :: [Name] -> Term -> Term; lams = flip (foldr Lam)
plusOne :: Term -> Term; plusOne x = App (App (Def "+") x) (Lit (LNat 1))
modThree :: Term -> Term; modThree x = App (App (Def "_%_") x) (Lit (LNat 3))
