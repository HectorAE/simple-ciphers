{-|
Module      : Crypto.XOR
Description : Simple XOR cipher
Copyright   : (c) Hector A Escobedo IV 2015
License     : GPL-3
Maintainer  : ninjahector.escobedo@gmail.com
Stability   : experimental
Portability : portable

A portable implementation of the repeating XOR cipher.
-}

module Crypto.XOR (
  xorChar,
  xorString,
  repeatingXorString,
  xorEncrypt,
  xorDecrypt
  ) where

import Data.Bits (xor)
import Data.Char (chr, ord)

import Data.List.Split (chunksOf)

xorChar :: Char -> Char -> Char
xorChar e k = chr $ ord e `xor` ord k

xorString :: String -> String -> String
xorString = zipWith xorChar

-- With XOR, ciphertext is the same as plaintext. Encryption is the same
-- operation as decryption.
repeatingXorString :: String -> String -> String
repeatingXorString key text = let sections = chunksOf (length key) text
                              in
                               concatMap (xorString key) sections

-- Synonyms
xorEncrypt = repeatingXorString
xorDecrypt = repeatingXorString
