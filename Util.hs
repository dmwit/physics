module Util where

import Control.Arrow
import Control.Monad
import Physics.Hipmunk

castFloat :: (Real a, Fractional b) => a -> b
castInt   :: (Integral a, Num b)    => a -> b
castFloat = fromRational . toRational
castInt   = fromInteger  . fromIntegral
castVector (Vector x y) = join (***) castFloat (x, y)
