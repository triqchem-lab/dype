{-# LANGUAGE OverloadedStrings #-}
-- | Dayan.Adapter.Agda — Agda 类型兼容适配层
--
-- Phase 4 的桥梁: 在大衍 AST 和 Agda.Syntax 之间建立类型映射。
-- 当前用独立定义 (轻量桩), Phase 4 后期替换为 vendor/agda-src 的真实类型。
--
-- vendor/agda-src 对应:
--   Range      → Agda.Syntax.Position.Range
--   ModuleName → Agda.Syntax.TopLevelModuleName
--   QName      → Agda.Syntax.Abstract.Name.QName

module Dayan.Adapter.Agda where

import Data.Text (Text)
import qualified Dayan.ProofGen.AST as D

----------------------------------------------------------------------
-- 1. Agda 兼容类型 (轻量定义)
----------------------------------------------------------------------

-- | Agda 区间 (对应 Agda.Syntax.Position.Range)
newtype AgdaRange = AgdaRange Text deriving (Show, Eq)

-- | Agda 限定名 (对应 Agda.Syntax.Abstract.Name.QName)
newtype AgdaQName = AgdaQName Text deriving (Show, Eq)

-- | Agda 模块名 (对应 Agda.Syntax.TopLevelModuleName)
newtype AgdaModuleName = AgdaModuleName Text deriving (Show, Eq)

----------------------------------------------------------------------
-- 2. 大衍 → Agda 类型转换
----------------------------------------------------------------------

-- | 大衍 AST → Agda QName
toAgdaQName :: D.Term -> Maybe AgdaQName
toAgdaQName (D.Def n) = Just (AgdaQName n)
toAgdaQName _         = Nothing

-- | 大衍模块名 → Agda 模块名
toAgdaModule :: Text -> AgdaModuleName
toAgdaModule = AgdaModuleName

-- | Agda 模块名 → 大衍 Text
fromAgdaModule :: AgdaModuleName -> Text
fromAgdaModule (AgdaModuleName n) = n

----------------------------------------------------------------------
-- 3. Type synonym bridge (Phase 4 full integration)
----------------------------------------------------------------------

-- | 当前用 Text, 后期用 Agda.Syntax.Abstract
type AgdaTerm = D.Term

-- | 当前用 Text, 后期用 Agda.Syntax.Internal
type AgdaType = D.Type

-- | 大衍 AST → Agda Internal (占位)
toAgdaTerm :: D.Term -> AgdaTerm
toAgdaTerm = id

toAgdaType :: D.Type -> AgdaType
toAgdaType = id

----------------------------------------------------------------------
-- 4. Parse.Dy 集成点
----------------------------------------------------------------------

-- | 解析 .dy 文件返回 (模块名, Agda声明列表)
--   后期替换为 Agda.Syntax.Parser 的真实解析
type DyParseResult = Either Text (AgdaModuleName, [D.Decl])
