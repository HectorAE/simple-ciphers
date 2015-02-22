-- Vigenère cipher library
-- Written by Hector Escobedo

module Crypto.Vigenere
       (
         vigenereEncrypt,
         vigenereDecrypt
       )
       where

import Crypto.Caesar
  (
    caesarEncrypt,
    caesarDecrypt,
    caesarStandardize,
    standardAlphabet
  )

import Data.List (elemIndex)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

-- Generic form to make life easier for us by not having to write everything
-- twice for encrypt/decrypt.
vigenereBy :: (Int -> String -> String) -> String -> String -> String
vigenereBy cipher key text = concatMap round sections
  where
    sections = chunksOf (length key) (caesarStandardize text)
    alphaIndex c = fromJust $ elemIndex c standardAlphabet
    caesarKeys = map alphaIndex $ caesarStandardize key
    round sec = concat $ zipWith cipher caesarKeys $ map (: []) sec

vigenereEncrypt :: String -> String -> String
vigenereEncrypt = vigenereBy caesarEncrypt

vigenereDecrypt :: String -> String -> String
vigenereDecrypt = vigenereBy caesarDecrypt
