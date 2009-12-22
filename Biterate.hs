


module Biterate where

import Data.List
import Data.Bits




biterate                    ::  (Bits b) => [b] -> b
biterate                     =  foldl' shift_complement 0
 where
  shift_complement a b       =  shiftL a 1 .|. b

