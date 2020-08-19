module ConverterTest where

import Converter

scprop_EncodeDecode :: String -> Bool
scprop_EncodeDecode str = str == (decode . encode) str
