{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- | Return all anagrams of the given string that appear in the given
-- dictionary file.
--
-- NOTE: list permutations contains duplicated but Set members is unique
-- >>> fastAnagrams (listh "apple") (listh "./share/dicts.txt")
-- ["appel","pepla","apple"]
--
fastAnagrams :: Chars
             -> FilePath
             -> IO (List Chars)
fastAnagrams ws pth = anagrams' . lines <$> readFile pth
  where anagrams' :: List Chars -> List Chars
        anagrams' = filter (\d -> NoCaseString d `S.member` permSet)
        permSet = S.fromList . hlist $  NoCaseString <$> permutations ws

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  } deriving Ord

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
