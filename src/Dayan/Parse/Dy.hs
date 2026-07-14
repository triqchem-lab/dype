{-# LANGUAGE OverloadedStrings #-}
module Dayan.Parse.Dy where

import Data.Text (Text)
import qualified Data.Text as T
import Dayan.ProofGen.AST
import Dayan.Adapter.Agda (AgdaModuleName(..))

parseDy :: Text -> Either String (AgdaModuleName, AgdaFile)
parseDy input =
  case T.lines input of
    []  -> Left "empty file"
    ls  -> case parseOpts (head ls) of
             Just opts -> Right (AgdaModuleName "TODO", AgdaFile opts "TODO" (parseDecls (tail ls)))
             Nothing   -> Right (AgdaModuleName "TODO", AgdaFile "" "TODO" (parseDecls ls))

parseOpts :: Text -> Maybe Text
parseOpts line
  | "{-#" `T.isPrefixOf` line && "#-}" `T.isSuffixOf` line = Just line
  | otherwise = Nothing

parseDecls :: [Text] -> [Decl]
parseDecls [] = []
parseDecls (l:ls)
  | "--" `T.isPrefixOf` l = DComment (T.drop 2 l) : parseDecls ls
  | "postulate" `T.isPrefixOf` l = parsePostulate l : parseDecls ls
  | "module" `T.isPrefixOf` l = parseDecls ls
  | "where" `T.isPrefixOf` l = parseDecls ls
  | T.null l = parseDecls ls
  | ":" `T.isInfixOf` l = parseDef l ls
  | otherwise = parseDecls ls

parsePostulate :: Text -> Decl
parsePostulate line =
  let parts = T.words line
      name = if length parts > 1 then parts !! 1 else "?"
  in DPostulate name TNat

parseDef :: Text -> [Text] -> [Decl]
parseDef typeLine rest =
  let (name, _) = splitColon typeLine
      ty = TDef "Set"
  in case rest of
       [] -> [DDef name ty [Clause [] Refl]]
       (bodyLine:more) ->
         let body = if "= refl" `T.isSuffixOf` bodyLine then Refl else Hole
         in DDef name ty [Clause [] body] : parseDecls more

splitColon :: Text -> (Text, Text)
splitColon t =
  case T.breakOn ":" t of
    (name, rest) -> (T.strip name, T.strip (T.drop 1 rest))
