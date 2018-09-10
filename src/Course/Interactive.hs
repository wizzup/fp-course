{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE LambdaCase #-}

module Course.Interactive where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.Traversable
import Course.List
import Course.Optional

-- | Eliminates any value over which a functor is defined.
--
-- >>> vooid Empty
-- Empty
--
-- >>> vooid (Full 10)
-- Full ()
--
vooid :: Functor m
       => m a
       -> m ()
vooid = (<$>) (const ())

-- | A version of @bind@ that ignores the result of the effect.
--
-- >>> Empty >- Full 1
-- Empty
--
-- >>> Full 1 >- (Full 'a')
-- Full 'a'
--
-- >>> pure False >- pure True
-- True
--
(>-) :: Monad m
     => m a
     -> m b
     -> m b
-- (>-) a = (>>=) a . const
-- a >- b = ((>>=) a . const) $ b
-- a >- b = (>>=) a . const $ b
-- a >- b = (>>=) a (const b)
a >- b = a >>= const b

-- | Runs an action until a result of that action satisfies a given predicate.
--
-- >>> untilM (Full <$> const False) Empty
-- Empty
--
-- >>> untilM (Full <$> even) (Full 2)
-- Full 2
--
untilM :: forall a m. Monad m
       => (a -> m Bool) -- ^ The predicate to satisfy to stop running the action.
       -> m a           -- ^ The action to run until the predicate satisfies.
       -> m a
-- untilM p a =
--   a >>= \r ->
--   p r >>= \q ->
--   if q
--     then
--       pure r
--     else
--       untilM p a

untilM p a = do
  r <- a
  q <- p r
  if q
  then pure r
  else untilM p a

-- | Example program that uses IO to echo back characters that are entered by the user.
echo :: IO ()
-- echo =
--   vooid (untilM
--           (\c ->
--             if c == 'q'
--               then
--                 putStrLn "Bye!" >-
--                 pure True
--               else
--                 pure False)
--           (putStr "Enter a character ('q' to exit): " >-
--            getChar >>= \c ->
--            putStrLn "" >-
--            putStrLn (c :. Nil) >-
--            pure c))

echo = vooid (untilM p g)
  where p 'q' = putStrLn "Bye!" >- pure True
        p _   = pure False
        g = do
          putStr "Enter a character ('q' to exit): "
          c <- getChar
          putStrLn ""
          putStrLn (c:.Nil)
          pure c


-- |
--
-- * Ask the user to enter a string to convert to upper-case.
--
-- * Convert the string to upper-case.
--
-- * Print the upper-cased string to standard output.
--
-- /Tip:/ @getLine :: IO String@ -- an IO action that reads a string from standard input.
--
-- /Tip:/ @toUpper :: Char -> Char@ -- (Data.Char) converts a character to upper-case.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.
--
convertInteractive :: IO ()
-- convertInteractive =
--   putStr "Enter a string: "  >-
--   getLine >>= (\ls ->
--   putStrLn $ "Upper case is :" ++ map toUpper ls)

convertInteractive = do
  putStr "Enter a string: "
  ls <- getLine
  putStrLn $ "Upper case is :" ++ map toUpper ls

-- |
--
-- * Ask the user to enter a file name to reverse.
--
-- * Ask the user to enter a file name to write the reversed file to.
--
-- * Read the contents of the input file.
--
-- * Reverse the contents of the input file.
--
-- * Write the reversed contents to the output file.
--
-- /Tip:/ @getLine :: IO String@ -- an IO action that reads a string from standard input.
--
-- /Tip:/ @readFile :: FilePath -> IO String@ -- an IO action that reads contents of a file.
--
-- /Tip:/ @writeFile :: FilePath -> String -> IO ()@ -- writes a string to a file.
--
-- /Tip:/ @reverse :: [a] -> [a]@ -- reverses a list.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.
--
reverseInteractive :: IO ()
-- reverseInteractive =
--   putStr "Enter input file path: " >-
--   getLine >>= \src ->
--   putStr "Enter output file path: " >-
--   getLine >>= \dst ->
--   readFile src >>= \ls ->
--   writeFile dst $ reverse ls

reverseInteractive = do
  putStr "Enter input file path: "
  src <- getLine
  putStr "Enter output file path: "
  dst <- getLine
  ls <- readFile src
  writeFile dst $ reverse ls

-- |
--
-- * Ask the user to enter a string to url-encode.
--
-- * Convert the string with a URL encoder.
--
-- * For simplicity, encoding is defined as:
--
-- * @' ' -> \"%20\"@
--
-- * @'\t' -> \"%09\"@
--
-- * @'\"' -> \"%22\"@
--
-- * @/anything else is unchanged/@
--
-- * Print the encoded URL to standard output.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.
--

-- | String encode function
--
-- >>> encode Nil
-- ""
--
-- >>> encode $ listh "a b c"
-- "a%20b%20c"
--
-- >>> encode $ listh "\"abc\""
-- "%22abc%22"
--
encode :: Chars -> Chars
-- encode = foldRight e Nil
--   where e ' ' cs  = listh "%20" ++ cs
--         e '\t' cs = listh "%09" ++ cs
--         e '"' cs  = listh "%22" ++ cs
--         e c cs   = c :. cs

-- (=<<) for List is flatMap :: (a -> List b) -> List a -> List b
--
-- λ> flatMap (\x -> (x:.x:.Nil) :. Nil) $ listh "abc"
-- ["aa","bb","cc"]
--
-- encode cs = e =<< cs
-- encode cs = (e =<<) cs
encode = (e =<<)
--
-- encode cs = cs >>= e
-- encode cs = (>>=) cs e
--
-- encode cs = join $ e <$> cs
  where e ' '  = "%20"
        e '\t' = "%09"
        e '"'  = "%22"
        e c = c :. Nil

encodeInteractive :: IO ()
encodeInteractive = do
  putStr "Enter URL: "
  ls <- getLine
  putStrLn $ encode ls

data Op = Op Char Chars (IO ()) -- keyboard entry, description, program

interactive ::
  IO ()
interactive =
  let ops = (
               Op 'c' "Convert a string to upper-case" convertInteractive
            :. Op 'r' "Reverse a file" reverseInteractive
            :. Op 'e' "Encode a URL" encodeInteractive
            :. Op 'q' "Quit" (pure ())
            :. Nil
            )
  in vooid (untilM
             (\c ->
               if c == 'q'
                 then
                   putStrLn "Bye!" >-
                   pure True
                 else
                   pure False)
             (putStrLn "Select: " >-
              traverse (\(Op c s _) ->
                putStr (c :. Nil) >-
                putStr ". " >-
                putStrLn s) ops >-
              getChar >>= \c ->
              putStrLn "" >-
              let o = find (\(Op c' _ _) -> c' == c) ops
                  r = case o of
                        Empty -> (putStrLn "Not a valid selection. Try again." >-)
                        Full (Op _ _ k) -> (k >-)
              in r (pure c)))
