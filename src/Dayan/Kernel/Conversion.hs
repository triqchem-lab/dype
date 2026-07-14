module Dayan.Kernel.Conversion where
import Data.Word (Word16, Word8)
import Dayan.Core.Tryte (Tryte(..), allTrytes)
import Dayan.Core.Torus (TorusPoint(..))
import Dayan.Compute.CRT (lookupPolar, lookupToroidal)
import Dayan.Compute.Orbit (a4Group, a4Action)

data Cmp = Equal | NotEqual deriving (Show, Eq)

compareTryte :: Tryte -> Tryte -> Cmp
compareTryte (Tryte a) (Tryte b) = if a == b then Equal else NotEqual

compareTorus :: TorusPoint -> TorusPoint -> Cmp
compareTorus (TorusPoint p1 t1) (TorusPoint p2 t2) =
  if p1 == p2 && t1 == t2 then Equal else NotEqual

orbitEqual :: Tryte -> Tryte -> Bool
orbitEqual a b = any (\g -> a4Action g a == b) a4Group

crtEqual :: Word16 -> Word16 -> Bool
crtEqual a b = lookupPolar a == lookupPolar b && lookupToroidal a == lookupToroidal b

forall729 :: (Tryte -> Bool) -> Bool
forall729 p = all p allTrytes
