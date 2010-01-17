
module AtomAVR.Firmware where

import Language.Atom

import AtomAVR.Elements




data ISR i where
  ISR                       ::  (Interrupt i) => i -> Atom () -> ISR i
instance (Show i) => Show (ISR i) where
  show (ISR i atom)          =  unwords ["ISR", show i, "<atom>"]

data Firmware i where
  Firmware :: (Interrupt i) => Atom () -> [ISR i] -> Firmware i
instance (Show i) => Show (Firmware i) where
  show (Firmware atom isrs)  =  unwords ["Firmware", "<atom>", show isrs]

