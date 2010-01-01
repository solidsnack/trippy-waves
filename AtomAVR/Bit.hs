
module AtomAVR.Bit where




newtype Bit                  =  Bit Bool
deriving instance Eq Bit
deriving instance Ord Bit
deriving instance Show Bit
instance Num Bit where
  (+)                        =  error "No arithmetic on bits."
  (-)                        =  error "No arithmetic on bits."
  (*)                        =  error "No arithmetic on bits."
  negate                     =  error "No negation of bits."
  signum                     =  error "Bits have no sign."
  abs                        =  id
  fromInteger 0              =  Bit False
  fromInteger 1              =  Bit True
  fromInteger _              =  error "Bits must be 0 or 1."


