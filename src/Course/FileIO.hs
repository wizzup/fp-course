{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, ,>>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)
  uncurry :: (a -> b -> c) -> (a, b) -> c
  curry :: ((a, b) -> c) -> a -> b -> c

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- $setup
-- >>> :set -XOverloadedStrings
--
-- | Given the file name, and file contents, print them.
-- Use @putStrLn@.
--
-- >>> printFile "/path/to/file" "file contents"
-- ============ /path/to/file
-- file contents
--
printFile ::
  FilePath
  -> Chars
  -> IO ()
-- printFile fp cs =
--     putStrLn ("============ " ++ fp) >>= (\_ ->
--     putStrLn cs)

-- printFile fp cs = do
--     putStrLn ("============ " ++ fp)
--     putStrLn cs

printFile fp cs =
    putStrLn ("============ " ++ fp) >>
    putStrLn cs

-- | Given a list of (file name and file contents pair), print each.
-- Use @printFile@.
--
-- >>> printFiles (("a","b") :. ("c","d") :. Nil)
-- ============ a
-- b
-- ============ c
-- d

printFiles ::
  List (FilePath, Chars)
  -> IO ()
-- using explicit recusion
-- printFiles Nil       = return ()
-- printFiles (l :. ls) = do
--   uncurry printFile l
--   printFiles ls
--
-- using foldRight
printFiles = foldRight f (pure ())
  where f :: (FilePath, Chars) -> IO () -> IO ()
        f l = (>>) $ uncurry printFile l

-- | Given a file name, return (file name and file contents).
-- Use @readFile@.
--
-- >>> getFile "share/a.txt"
-- ("share/a.txt","the contents of a\n")
--
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile fp = readFile fp >>= (\x -> return (fp, x))

-- | Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
--
-- >>> getFiles ("share/a.txt" :. "share/b.txt" :. Nil)
-- [("share/a.txt","the contents of a\n"),("share/b.txt","the contents of b\n")]
--
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))

-- using explicit recusion
-- getFiles Nil       = pure Nil
-- getFiles (f :. fs) = lift2 (:.) (getFile f) (getFiles fs)
-- getFiles (f :. fs) = (:.) <$> getFile f <*> getFiles fs
-- getFiles (f :. fs) = pure (:.) <*> getFile f <*> getFiles fs

-- using foldRight
-- getFiles = foldRight f (pure Nil)
--   where f :: FilePath -> IO (List (FilePath, Chars)) -> IO (List (FilePath, Chars))
--         -- f l = lift2 (:.) (getFile l)
--         -- f l = (lift2 (:.) . getFile) $ l
--         f = lift2 (:.) . getFile

-- using high-order functions composition
-- getFiles ls = sequence (getFile <$> ls)
-- getFiles ls = sequence ((<$>) getFile ls)
getFiles = sequence . (<$>) getFile

-- | Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.
--
-- >>> run "share/files.txt"
-- ============ share/a.txt
-- the contents of a
-- <BLANKLINE>
-- ============ share/b.txt
-- the contents of b
-- <BLANKLINE>
-- ============ share/c.txt
-- the contents of c
-- <BLANKLINE>
--
run ::
  FilePath
  -> IO ()
run fp = words <$> readFile fp
     >>= getFiles
     >>= printFiles

-- | To test this module, load ghci in the root of the project directory, and do
-- /Tip:/ use @getArgs@ and @run@
--
-- >>> :main "share/files.txt"
-- ============ share/a.txt
-- the contents of a
-- <BLANKLINE>
-- ============ share/b.txt
-- the contents of b
-- <BLANKLINE>
-- ============ share/c.txt
-- the contents of c
-- <BLANKLINE>
--

main ::
  IO ()
main = flatten <$> getArgs >>= run
