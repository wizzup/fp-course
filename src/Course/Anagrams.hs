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

-- | Return all anagrams of the given string that appear in the given
-- dictionary file.
--
-- >>> anagrams (listh "apple") (listh "./share/dicts.txt")
-- ["apple","apple","pepla","pepla","appel","appel"]
--
anagrams :: Chars            -- ^ A word to find anagrams of
         -> FilePath         -- ^ Path to dictionary file
         -> IO (List Chars)  -- ^ List of anagrams of a given words
anagrams ws pth = anagrams' ws . lines <$> readFile pth
  where anagrams' :: Chars -> List Chars -> List Chars
        anagrams' Nil _  = Nil
        anagrams' cs  ds = intersectBy equalIgnoringCase (permutations cs) ds

-- | Compare two strings for equality, ignoring case
--
-- >>> equalIgnoringCase (listh "apple") (listh "APPle")
-- True
--
-- >>> equalIgnoringCase (listh "apple") (listh "banana")
-- False
--
equalIgnoringCase :: Chars
                  -> Chars
                  -> Bool
-- equalIgnoringCase as bs = (map toLower as) == (map toLower bs)
-- equalIgnoringCase as bs = (==) (map toLower as) (map toLower bs)
-- equalIgnoringCase as bs = ((==) `on` (map toLower)) as bs
equalIgnoringCase = (==) `on` map toLower
