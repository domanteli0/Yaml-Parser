module Document(Document(..)) where
import Data.List as L (sort)

data Document = 
    DNull
  | DInt Int
  | DFloat Double
  | DString String
  | DList [Document]
  | DMap [(String, Document)]
  deriving (Show, Ord)

instance Eq Document where
  (==) = cmp

cmp :: Document -> Document -> Bool
DNull        `cmp` DNull        = True
(DInt i1)    `cmp` (DInt i2)    = i1 == i2
(DFloat f1)  `cmp` (DFloat f2)  = f1 == f2
(DInt i)     `cmp` (DFloat f)   = f == fromIntegral i
(DFloat f)   `cmp` (DInt i)     = fromIntegral i == f
(DString s1) `cmp` (DString s2) = s1 == s2
(DList l1)   `cmp` (DList l2)   = l1 == l2
(DMap m1)    `cmp` (DMap m2)    = L.sort m1 == L.sort m2
_            `cmp` _            = False

