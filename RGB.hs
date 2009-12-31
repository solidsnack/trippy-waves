#!/usr/bin/env runhaskell

import Control.Applicative
import Data.List (foldl')
import Data.Word

import Language.Atom

import Biterate
import qualified AVR





{-| We are outputting on @PORTB@, the eight pins coming out of the
    ATtiny25 chip. We need to set the pins to be input or output, as
    appropriate; this is done with the @DDRB@ register. Then we set all the
    pins to high by maxing out the @PORTB@ register.
 -}
set_up_pins                  =  "set_up_pins" `atom` do
    {-  The one byte register describing the "directions" -- in and out -- of
     -  the pins. A @0@ indicates a pin is input; a @1@ indicates output.
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    directions              <-  word8' "DDRB"
    directions <== biterate
      [ 0   --  PORTB7 is read only, per the ATtiny25 datasheet.
      , 0   --  PORTB6 is read only, per the ATtiny25 datasheet.
      , 0   --  PORTB5 does nothing in this application.
      , 1   --  PORTB4 is the blue LED.
      , 0   --  PORTB3 is the infrared detector.
      , 1   --  PORTB2 is the green LED.
      , 1   --  PORTB1 is the red LED.
      , 1 ] --  PORTB0 is the infrared emitter.
    pins                    <-  word8' "PORTB"
    pins <== biterate [1,1,1,1,1,1,1,1] -- Turns LEDs off.

--    -- Initialization of external vars.
--    initialize              <-  bool "initialize" True


--    -- Is the LED currently on? (Assume it starts False/off)
--    isOn                    <-  bool "isOn" False

--    -- Does the toggle counter need a reset? (Assume it starts False/no)
--    doReset                 <-  bool "doReset" False

--    -- Initialize the toggle counter to delayCycles
--    toggle                  <-  word16 "toggle" delayCycles

--    -- Initialize the pin
--    period 1 $ atom "init" $ do
--      cond $ value initialize
--      dDRD       <== 255
--      initialize <== false


main                         =  compile "main" AVR.config set_up_pins

