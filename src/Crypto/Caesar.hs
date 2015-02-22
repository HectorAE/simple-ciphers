-- Caesar cipher library
-- Written by Hector Escobedo

module Crypto.Caesar
       (
         standardAlphabet,
         caesarStandardize,
         caesarEncryptChar,
         caesarEncrypt,
         caesarDecrypt,
         rot13
       )
       where

import Data.Char (isAlpha, isUpper, toUpper)
import Data.List (elemIndex)
import Data.Maybe (fromJust, mapMaybe)

-- A helper function for shifting lists by any integer value
shift :: Int -> [a] -> [a]
shift n xs
  | normalN < 0 = snd leftShifted ++ fst leftShifted
  | otherwise = snd rightShifted ++ fst rightShifted
  where
    normalN = rem n (length xs)
    rightShifted = splitAt normalN xs
    leftShifted = splitAt (length xs + normalN) xs

standardAlphabet :: String
standardAlphabet = ['A'..'Z']

caesarStandardize :: String -> String
caesarStandardize = map toUpper . filter isAlpha

-- Encrypt a single character, but do not attempt to standardize it
caesarEncryptChar :: Int -> Char -> Maybe Char
caesarEncryptChar key c
  | isAlpha c && isUpper c = Just $ shiftAlphabet !!
                             fromJust (elemIndex c standardAlphabet)
  | otherwise = Nothing
  where
    shiftAlphabet = shift key standardAlphabet

-- Right shift the plaintext
caesarEncrypt :: Int -> String -> String
caesarEncrypt key text = mapMaybe (caesarEncryptChar key) standardText
  where
    standardText = caesarStandardize text

-- Left shift the ciphertext
caesarDecrypt :: Int -> String -> String
caesarDecrypt key = caesarEncrypt (- key)

-- Common Caesar cipher usage
rot13 :: String -> String
rot13 = caesarEncrypt 13
