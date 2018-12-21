{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad

-- for debuging
import qualified Prelude as P
import qualified Control.Monad as M

-- $setup
-- >>> :set -XOverloadedStrings

-- | The representation of the grouping of each exponent of one thousand.
-- ["thousand", "million", ...]
--
illion :: List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- | A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  -- deriving (Eq, Ord)
  deriving (Eq, Ord, Show, P.Enum)

-- | convert `Digit` to showable `Chars`
--
showDigit :: Digit
          -> Chars
showDigit Zero  = "zero"
showDigit One   = "one"
showDigit Two   = "two"
showDigit Three = "three"
showDigit Four  = "four"
showDigit Five  = "five"
showDigit Six   = "six"
showDigit Seven = "seven"
showDigit Eight = "eight"
showDigit Nine  = "nine"

-- | A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 = D1 Digit
            | D2 Digit Digit
            | D3 Digit Digit Digit
  -- deriving Eq
  deriving (Eq, Show)

-- | Possibly convert a character to a digit.
--
fromChar :: Char
         -> Optional Digit
fromChar '0' = Full Zero
fromChar '1' = Full One
fromChar '2' = Full Two
fromChar '3' = Full Three
fromChar '4' = Full Four
fromChar '5' = Full Five
fromChar '6' = Full Six
fromChar '7' = Full Seven
fromChar '8' = Full Eight
fromChar '9' = Full Nine
fromChar _   = Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
--
-- dollars :: Chars
--         -> Chars
dollars w = ds ++ cs
  where (d,c) = splitDot w
        d' = groupDigits d
        c' = groupDigits c
        ds = map sayDigits d'
        cs = map sayDigits c'

-- | Split dollar and cent portion, return list of Digit
--
-- >>> splitDot "0"
-- >>> splitDot "0.1"
-- >>> splitDot "1."
-- >>> splitDot ".34"
-- >>> splitDot "0.3456789"
-- >>> splitDot "a1a"
-- >>> splitDot "a1a.a0.7b"
-- >>> splitDot "100.0"
-- >>> splitDot "100.00"
-- >>> splitDot "100.00000"
-- >>> splitDot "1001456.13"
-- >>> splitDot "9abc9def9ghi.jkl9mno"
--
splitDot :: Chars -> (List Digit, List Digit)
splitDot w = (h d, take 2 $ h c')
  where d = takeWhile (/= '.') w
        c = drop (length d) w
        c' = case c of
               Nil -> '0' :. Nil
               (a:.Nil) -> a :. Nil
               (a:.b:._) -> a :. b :. Nil
        f xs = filter isFull $ map fromChar xs
        g (Full x) = x
        g Empty = undefined -- suppress incomplete pattern warning
                            -- should not ever been called
        isFull Empty    = False
        isFull (Full _) = True
        h = map g . f

groupDigits :: List Digit -> List Digit3
groupDigits (a :. b :. c :. ds) = D3 a b c :. groupDigits ds
groupDigits (b :. c :. ds) = D2  b c :. groupDigits ds
groupDigits (c :. ds) = D1 c :. groupDigits ds
groupDigits Nil = Nil

sayDigits :: Digit3 -> Chars
sayDigits (D1 a)    = showDigit a
sayDigits (D2 Zero a) = sayDigits $ D1 a
sayDigits (D2 One  Zero) = "ten"
sayDigits (D2 One  One) = "eleven"
sayDigits (D2 One  Two) = "twelve"
sayDigits (D2 One  Three) = "thirteen"
sayDigits (D2 One  Four) = "fourteen"
sayDigits (D2 One  Five) = "fiftteen"
sayDigits (D2 One  Six) = "sixteen"
sayDigits (D2 One  Seven) = "seventeen"
sayDigits (D2 One  Eight) = "eightteen"
sayDigits (D2 One  Nine) = "nineteen"
sayDigits (D2 Two  Zero) = "twenty"
sayDigits (D2 Two  a) = sayDigits (D2 Two Zero) ++ "-" ++ sayDigits (D1 a)
sayDigits (D2 Three  Zero) = "thirty"
sayDigits (D2 Three  a) = sayDigits (D2 Three Zero) ++ "-" ++ sayDigits (D1 a)
sayDigits (D2 Five  Zero) = "fifty"
sayDigits (D2 Five  a) = sayDigits (D2 Five Zero) ++ "-" ++ sayDigits (D1 a)
sayDigits (D2 Eight  Zero) = "eighty"
sayDigits (D2 Eight  a) = sayDigits (D2 Five Zero) ++ "-" ++ sayDigits (D1 a)
sayDigits (D2 b  Zero) = showDigit b ++ "ty"
sayDigits (D2 b    a) = showDigit b ++"ty-" ++ showDigit a
sayDigits (D3 Zero Zero a)    = sayDigits $ D1 a
sayDigits (D3 Zero b    a)    = sayDigits $ D2 b a
sayDigits (D3 c    Zero Zero) = showDigit c ++ " hundred"
sayDigits (D3 c    b    a)    = sayDigits (D3 c Zero Zero) ++ " and " ++ sayDigits (D2 b a)

runOptional :: Optional a -> a
runOptional Empty = undefined -- should not be call
runOptional (Full a) = a -- should not be call

ss :: List Digit
ss = map (runOptional . fromChar) $ listh "0123456789"

mm :: M.Monad m => (a -> m b) -> [a] -> m ()
mm = M.mapM_

pp :: Show a => a -> IO ()
pp = P.print

mp :: Show a => List a -> IO ()
mp = mm pp . hlist
-- >>> mp $ map (sayDigits . D1) ss
-- >>> mp $ map (\s -> map (sayDigits . D2 s) ss) ss
-- >>> mp $ map (\s -> map (sayDigits . D3 Zero s) ss) ss
-- >>> mp $ map (\s -> map (sayDigits . D3 One s) ss) ss
-- >>> mp $ map (\s -> map (sayDigits . D3 Two s) ss) ss

