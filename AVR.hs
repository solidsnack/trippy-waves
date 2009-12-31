
module AVR where

import Control.Applicative
import Data.Word

import Language.Atom




includes                     =  ("#include <" ++) . (++ ">") . ("avr/" ++) <$>
  [ "io.h", "interrupt.h", "sleep.h", "pgmspace.h" ]


types Bool                   =  "uint8_t"
types Int8                   =  "int8_t"
types Int16                  =  "int16_t"
types Int32                  =  "int32_t"
types Int64                  =  "int64_t"
types Word8                  =  "uint8_t"
types Word16                 =  "uint16_t"
types Word32                 =  "uint32_t"
types Word64                 =  "uint64_t"
types Float                  =  "float"
types Double                 =  "double"


config                      ::  Config
config                       =  defaults { cType = types }


