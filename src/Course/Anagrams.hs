{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* (<$>) :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Chars
* lines :: Chars -> List Chars
* permutations :: List a -> List (List a)
* intersectBy :: (a -> b -> Bool) -> List a -> List b -> List a
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
anagrams =
  error "todo: Course.Anagrams#anagrams"

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase =
  error "todo: Course.Anagrams#equalIgnoringCase"
