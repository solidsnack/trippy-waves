
module Blink where

import AtomAVR.Firmware
import AtomAVR.ATtiny25

import Language.Atom




firmware                     =  Firmware (open_pins >> set_up_clock_interrupt)
                                         [ISR TIMER0_OVF blink]


blink                        =  on >> off
 where
  on                         =  (phase 0 . period 8) lo
  off                        =  (phase 7 . period 8) hi

lo                           =  do
  portb                     <-  word8' "PORTB"
  portb <== 0x00

hi                           =  do
  portb                     <-  word8' "PORTB"
  portb <== 0x17            --  00010111


open_pins                    =  do
  ddrb                      <-  word8' "DDRB"
  ddrb <== 0x17             --  00010111

set_up_clock_interrupt       =  do
  tccr0b                    <-  word8' "TCCR0B"
  tccr0b <== 0x05           --  Every tick shall be 1024 timer cycles.
  timsk0                    <-  word8' "TIMSK0"
  timsk0 <== 0x01           --  Enable the time overflow interrupt.
  tcnt0                     <-  word8' "TCNT0"
  tcnt0 <== 0x00            --  Zero out the clock.
  action (const "sei()") []

