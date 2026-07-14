{-# LANGUAGE OverloadedStrings #-}
module Dayan.Parse.Dy where
import Data.Text (Text)
import qualified Data.Text as T
import Dayan.ProofGen.AST
import Dayan.Adapter.Agda (AgdaModuleName(..))

parseDy :: Text -> Either String (AgdaModuleName, AgdaFile)
parseDy input = case T.lines input of
  []    -> Left "empty file"
  (l:ls) -> case parseOpts l of
    Just opts -> Right (AgdaModuleName "TODO", AgdaFile opts "TODO" (parseDecls ls))
    Nothing   -> Right (AgdaModuleName "TODO", AgdaFile "" "TODO" (parseDecls ls))

parseOpts l | "{-#" `T.isPrefixOf` l && "#-}" `T.isSuffixOf` l = Just l
            | otherwise = Nothing

parseDecls [] = []
parseDecls (l:ls)
  | "--" `T.isPrefixOf` l = DComment (T.drop 2 l) : parseDecls ls
  | "postulate" `T.isPrefixOf` l = parsePostulate l : parseDecls ls
  | "module" `T.isPrefixOf` l = parseDecls ls
  | "where" `T.isPrefixOf` l = parseDecls ls
  | T.null l = parseDecls ls
  | ":" `T.isInfixOf` l = parseDef l ls
  | otherwise = parseDecls ls

parsePostulate line = DPostulate name TNat where
  parts = T.words line
  name = if length parts > 1 then parts !! 1 else "?"

parseDef typeLine rest = case rest of
  [] -> [DDef name ty [Clause [] Refl]]
  (bodyLine:more) -> DDef name ty [Clause [] body] : parseDecls more
    where body = if "= refl" `T.isSuffixOf` bodyLine then Refl else Hole
  where (name, _) = splitColon typeLine; ty = TDef "Set"

splitColon t = case T.breakOn ":" t of (name, rest) -> (T.strip name, T.strip (T.drop 1 rest))
