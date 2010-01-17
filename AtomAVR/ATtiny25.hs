
module AtomAVR.ATtiny25 where

import qualified AtomAVR.Elements as Elements




data Interrupt
  = RESET
  | INT0
  | PCINT0
  | TIMER1_COMPA
  | TIMER1_OVF
  | TIMER0_OVF
  | EE_RDY
  | ANA_COMP
  | ADC
  | TIMER1_COMPB
  | TIMER0_COMPA
  | TIMER0_COMPB
  | WDT
  | USI_START
  | USI_OVF
deriving instance Eq Interrupt
deriving instance Ord Interrupt
deriving instance Show Interrupt
instance Elements.Interrupt Interrupt where
  name                       =  ("_vect" ++) . show


