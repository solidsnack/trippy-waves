#!/usr/bin/env runhaskell

import Data.Word
import Foreign
import Foreign.C




foreign import ccall safe "addresses.h PORTB" rPORTB :: Ptr Word8
foreign import ccall safe "addresses.h DDRB" rDDRB :: Ptr Word8



lights_out                   =  poke rPORTB 0x17 -- 0001.0111

blue                         =  poke rPORTB 0x07 -- 0000.0111

green                        =  poke rPORTB 0x13 -- 0001.0011

red                          =  poke rPORTB 0x15 -- 0001.0101


directions                   =  poke rDDRB 0x17   --  0 0 0 1 0 1 1 1
                                                  --        | | | | IR LED.
                                                  --        | | | Red LED.
                                                  --        | | Green LED.
                                                  --        | IR sensor.
                                                  --        Blue LED.


main                         =  do
  directions
  lights_out
  return ()
  


