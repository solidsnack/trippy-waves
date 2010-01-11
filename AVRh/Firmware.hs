
module AVRh.Firmware where

import Language.Atom




data Firmware                =  Firmware [ISR] [DECL] (Atom ())


data Interrupt               =  Interrupt


data ISR                     =  ISR Interrupt (Atom ())


data DECL                    =  PROGMEM | GLOBAL


class Compile t where
  compile                   ::  Config -> t -> String
instance Compile ISR where
  compile = undefined
instance Compile DECL where
  compile = undefined
instance Compile Firmware where
  compile conf (Firmware theISRs theDECLs main_routine) =
    compile' theDECLs ++ compile' theISRs ++ atom_compile main_routine
 where
  compile' = fmap (compile conf)
  atom_compile = undefined


