#!/usr/bin/env runhaskell

import Control.Applicative
import Data.List (foldl')
import Data.Word

import Language.Atom

import Biterate




{-| We are outputting on @PORTB@, the eight pins coming out of the
    ATtiny25 chip. We need to set the pins to be input or output, as
    appropriate; this is done with the @DDRB@ register. Then we turn out all
    the lights.
 -}
set_up_pins                  =  "set_up_pins" `atom` do
  {-  The one byte register describing the "directions" -- in and out -- of
   -  the pins. A @0@ indicates a pin is input; a @1@ indicates output.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  directions                <-  word8' "DDRB"
  directions <== biterate
    [ 0   --  PORTB7 is read only, per the ATtiny25 datasheet.
    , 0   --  PORTB6 is read only, per the ATtiny25 datasheet.
    , 0   --  PORTB5 does nothing in this application.
    , 1   --  PORTB4 is the blue LED.
    , 0   --  PORTB3 is the infrared detector.
    , 1   --  PORTB2 is the green LED.
    , 1   --  PORTB1 is the red LED.
    , 1 ] --  PORTB0 is the infrared emitter.
  ports                     <-  word8' "PORTB"
  ports <== biterate [0,0,0,1,0,1,1,1] -- Turns LEDs off.


{-| Drawn from work of John Van Enk:

 <http://github.com/sw17ch/atom-arduino-experiments/blob/master/Blink/blink.c>

    Cargo-cult programming.
 -}
set_up_timer                 =  "set_up_timer" `atom` do
  timer_control             <-  word8' "TCCR0B"
  timer_control <== biterate [0,0,0,0,0,0,1,1] -- What does it do?
  timer_mask                <-  word8' "TIMSK0"
  timer_mask <== biterate [0,0,0,0,0,0,1,0] -- What does it do?
  timer_count               <-  word8' "TCNT0"
  timer_count <== biterate [0,0,0,0,0,1,0,1] -- What does it do?


blue_LED_exclusive           =  "blue" `atom` do
  ports                     <-  word8' "PORTB"
  ports <== biterate [0,0,0,0,0,1,1,1]


red_LED_exclusive            =  "red" `atom` do
  ports                     <-  word8' "PORTB"
  ports <== biterate [0,0,0,1,0,1,0,1]


green_LED_exclusive          =  "green" `atom` do
  ports                     <-  word8' "PORTB"
  ports <== biterate [0,0,0,1,0,0,1,1]


alternating_LEDs             =  "alternate" `atom` do
  period interval blue_LED_exclusive
  period interval red_LED_exclusive
  period interval green_LED_exclusive
 where
  interval                   =  100




main                         =  compile "main" config set_up
 where
  set_up                     =  set_up_pins >> set_up_timer


just_incl _ _ _              =  (unlines includes, "")
 where
  includes                   =  ("#include <" ++) . (++ ">") . ("avr/" ++) <$>
    [ "io.h", "interrupt.h", "sleep.h", "pgmspace.h" ]


config                      ::  Config
config                       =  defaults { cType = c99Types
                                         , cCode = just_incl
                                         , cAssert = False
                                         , cRuleCoverage = False
                                         }


