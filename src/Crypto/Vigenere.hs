{-|
Module      : Crypto.Vigenere
Description : Vigenère cipher
Copyright   : (c) Hector A Escobedo IV 2015
License     : GPL-3
Maintainer  : ninjahector.escobedo@gmail.com
Stability   : experimental
Portability : portable

A portable implementation of the Vigenère cipher.
-}

module Crypto.Vigenere (
  vigenereEncrypt,
  vigenereDecrypt
  ) where

import Crypto.Caesar (
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
vigenereBy cipher key text
  | null key = caesarStandardize text
  | otherwise = concatMap cipherSection sections
  where
    sections = chunksOf (length caesarKeys) (caesarStandardize text)
    alphaIndex c = fromJust $ elemIndex c standardAlphabet
    caesarKeys = map alphaIndex $ caesarStandardize key
    cipherSection sec = concat $ zipWith cipher caesarKeys $ map (: []) sec

vigenereEncrypt :: String -> String -> String
vigenereEncrypt = vigenereBy caesarEncrypt

vigenereDecrypt :: String -> String -> String
vigenereDecrypt = vigenereBy caesarDecrypt
