module Main where

import WebNote
import GHC.IO.Encoding
import GHC.IO.Encoding (utf8)

main :: IO ()
main = do
    setLocaleEncoding utf8
    note