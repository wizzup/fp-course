{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Parser where

import Course.Core
import Course.Person
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.List
import Course.Optional
import Data.Char

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

type Input = Chars

data ParseResult a = UnexpectedEof
                   | ExpectedEof Input
                   | UnexpectedChar Char
                   | UnexpectedString Chars
                   | Result Input a
  deriving Eq

instance Show a => Show (ParseResult a) where
  show UnexpectedEof =
    "Unexpected end of stream"
  show (ExpectedEof i) =
    stringconcat ["Expected end of stream, but got >", show i, "<"]
  show (UnexpectedChar c) =
    stringconcat ["Unexpected character: ", show [c]]
  show (UnexpectedString s) =
    stringconcat ["Unexpected string: ", show s]
  show (Result i a) =
    stringconcat ["Result >", hlist i, "< ", show a]

instance Functor ParseResult where
  (<$>) :: (a -> b)
        -> ParseResult a
        -> ParseResult b
  _ <$> UnexpectedEof      = UnexpectedEof
  _ <$> ExpectedEof i      = ExpectedEof i
  _ <$> UnexpectedChar c   = UnexpectedChar c
  _ <$> UnexpectedString s = UnexpectedString s
  f <$> Result i a         = Result i (f a)

-- | Function to determine is a parse result is an error.
isErrorResult :: ParseResult a
              -> Bool
isErrorResult (Result _ _)         = False
isErrorResult UnexpectedEof        = True
isErrorResult (ExpectedEof _)      = True
isErrorResult (UnexpectedChar _)   = True
isErrorResult (UnexpectedString _) = True

-- | Runs the given function on a successful parse result. Otherwise return the same failing parse result.
onResult :: ParseResult a
         -> (Input -> a -> ParseResult b)
         -> ParseResult b
onResult UnexpectedEof _         = UnexpectedEof
onResult (ExpectedEof i) _       = ExpectedEof i
onResult (UnexpectedChar c) _    = UnexpectedChar c
onResult (UnexpectedString s)  _ = UnexpectedString s
onResult (Result i a) k          = k i a

-- | A parser contain a function from Input to ParseResult
--
newtype Parser a = P (Input -> ParseResult a)

parse :: Parser a
      -> Input
      -> ParseResult a
parse (P p) = p

-- | Produces a parser that always fails with @UnexpectedChar@ using the given character.
unexpectedCharParser :: Char
                     -> Parser a
-- unexpectedCharParser c = P (\_ -> UnexpectedChar c)
-- unexpectedCharParser c = constantParser (UnexpectedChar c)
-- unexpectedCharParser c = constantParser $ UnexpectedChar c
-- unexpectedCharParser c = (constantParser . UnexpectedChar) c
unexpectedCharParser = constantParser . UnexpectedChar

unexpectedStringParser :: Chars
                       -> Parser a
unexpectedStringParser = constantParser . UnexpectedString

--- | Return a parser that always returns the given parse result.
---
--- >>> isErrorResult (parse (constantParser UnexpectedEof) "abc")
--- True
--
constantParser :: ParseResult a
               -> Parser a
constantParser = P . const

-- | Return a parser that succeeds with a character off the input or fails with an error if the input is empty.
--
-- >>> parse character "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse character "")
-- True
--
character :: Parser Char
character = P f
  where f Nil     = UnexpectedEof
        f (x:.xs) = Result xs x

-- | Parsers can map.
-- Write a Functor instance for a @Parser@.
--
-- >>> parse (toUpper <$> character) "amz"
-- Result >mz< 'A'
--
instance Functor Parser where
  (<$>) :: (a -> b)
        -> Parser a
        -> Parser b
  -- (<$>) = undefined
  -- f <$> P pf = P (\i -> (<$>) f (pf i))
  -- f <$> P pf = P (\i -> f <$> pf i)
  -- f <$> P pf = P (\i -> ((f <$>) . pf) i)
  f <$> P pf = P $ (f <$>) . pf

-- | Return a parser that always succeeds with the given value and consumes no input.
--
-- >>> parse (valueParser 3) "abc"
-- Result >abc< 3
--
valueParser :: a
            -> Parser a
-- valueParser a = P (\i -> Result i a)
-- valueParser a = P (\i -> i `Result` a)
valueParser a = P (`Result` a)

-- | Return a parser that tries the first parser for a successful value.
--
--   * If the first parser succeeds then use this parser.
--
--   * If the first parser fails, try the second parser.
--
-- >>> parse (character ||| valueParser 'v') Nil
-- Result >< 'v'
--
-- >>> parse (constantParser UnexpectedEof ||| valueParser 'v') Nil
-- Result >< 'v'
--
-- >>> parse (character ||| valueParser 'v') "abc"
-- Result >bc< 'a'
--
-- >>> parse (constantParser UnexpectedEof ||| valueParser 'v') "abc"
-- Result >abc< 'v'
--
(|||) :: forall a. Parser a
      -> Parser a
      -> Parser a
(|||) pa pb = P pr
  where pr, par, pbr :: Input -> ParseResult a
        par = parse pa
        pbr = parse pb
        -- pr i  = if isErrorResult (par i)
        --         then pbr i
        --         else par i
        pr i = bool par pbr (isErrorResult (par i)) i

infixl 3 |||

-- | Parsers can bind.
-- Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- >>> parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "abc"
-- Result >bc< 'v'
--
-- >>> parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "a"
-- Result >< 'v'
--
-- >>> parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "xabc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "")
-- True
--
-- >>> isErrorResult (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "x")
-- True
--
instance Monad Parser where
  (=<<) :: forall a b. (a -> Parser b)
        -> Parser a
        -> Parser b
  f =<< P pa = P pr
    where g :: Input -> a -> ParseResult b
          g j x = parse (f x) j
          pr :: Input -> ParseResult b
          pr i = onResult (pa i) g

-- | Write an Applicative functor instance for a @Parser@.
-- /Tip:/ Use @(=<<)@.
--
instance Applicative Parser where
  pure :: a
       -> Parser a
  pure = valueParser

  (<*>) :: forall a b. Parser (a -> b)
        -> Parser a
        -> Parser b

  -- pf <*> pa = pf >>= f
  --   where f :: (a -> b) -> Parser b
  --         -- f ab = ab <$> pa
  --         -- f ab = (<$>) ab pa
  --         f ab = (<$> pa) ab
  pf <*> pa = pf >>= (<$> pa)

  -- NOTE: This version is wrong binding direction
  -- using this version make `sequenceParser /= sequence`
  -- remaining is lost?
  -- pf <*> pa = f =<< pa
  --   where f :: a -> Parser b
  --         -- f a = (\x -> x $ a) <$> pf
  --         -- f a = (\x -> ($) x a) <$> pf
  --         -- f a = (\x -> ($ a) $ x) <$> pf
  --         f a = ($ a) <$> pf


-- | Return a parser that continues producing a list of values from the given parser.
--
-- /Tip:/ Use @list1@, @pure@ and @(|||)@.
--
-- >>> parse (list character) Nil
-- Result >< ""
--
-- >>> parse (list digit) "123abc"
-- Result >abc< "123"
--
-- >>> parse (list digit) "abc"
-- Result >abc< ""
--
-- >>> parse (list character) "abc"
-- Result >< "abc"
--
-- >>> parse (list (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> parse (list (character *> valueParser 'v')) ""
-- Result >< ""
--
list :: Parser a
     -> Parser (List a)
list pa = list1 pa ||| pure Nil

-- | Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
--
-- /Tip:/ Use @(=<<)@, @list@ and @pure@.
--
-- >>> parse (list1 (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (list1 (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (list1 (character *> valueParser 'v')) "")
-- True
--
list1 :: Parser a
      -> Parser (List a)
-- list1 pa = pa >>= (\a -> list pa >>= (\as -> pure $ a :. as))
list1 pa = do
  a <- pa
  as <- list pa
  pure $ a :. as

-- | Return a parser that produces a character but fails if
--
--   * The input is empty.
--
--   * The character does not satisfy the given predicate.
--
-- /Tip:/ The @(=<<)@, @unexpectedCharParser@ and @character@ functions will be helpful here.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult $ parse (satisfy isLower) Nil
-- True
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
--
satisfy :: (Char -> Bool)
        -> Parser Char
satisfy p = pc =<< character
  where pc :: Char -> Parser Char
        -- pc c | p c = pure c
        --      | otherwise = unexpectedCharParser c
        pc c = bool unexpectedCharParser pure (p c) c

-- | Return a parser that produces the given character but fails if
--
--   * The input is empty.
--
--   * The produced character is not equal to the given character.
--
-- /Tip:/ Use the @satisfy@ function.
--
-- >>> parse (is 'a') "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult $ parse (is 'a') Nil
-- True
--
-- >>> isErrorResult $ parse (is 'a') "xbc"
-- True
--
is :: Char
   -> Parser Char
-- is c = satisfy (== c)
is = satisfy . (==)

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * The input is empty.
--
--   * The produced character is not a digit.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isDigit@ functions.
--
-- >>> parse digit "123"
-- Result >23< '1'
--
-- >>> isErrorResult $ parse digit Nil
-- True
--
-- >>> isErrorResult $ parse digit "abc"
-- True
--
digit :: Parser Char
digit = satisfy isDigit

--
-- | Return a parser that produces a space character but fails if
--
--   * The input is empty.
--
--   * The produced character is not a space.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isSpace@ functions.
--
-- >>> parse space " bc"
-- Result >bc< ' '
--
-- >>> isErrorResult $ parse space Nil
-- True
--
-- >>> isErrorResult $ parse space "abc"
-- True
--
space :: Parser Char
space = satisfy isSpace

-- | Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--
--   * The input is empty.
--
--   * The first produced character is not a space.
--
-- /Tip:/ Use the @list1@ and @space@ functions.
--
-- >>> parse spaces1 "  abc"
-- Result >abc< "  "
--
-- >>> isErrorResult $ parse spaces1 Nil
-- True
--
-- >>> isErrorResult $ parse spaces1 "abc"
-- True
--
spaces1 :: Parser Chars
spaces1 = list1 space

-- | Return a parser that produces a lower-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not lower-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isLower@ functions.
--
-- >>> parse lower "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult $ parse lower Nil
-- True
--
-- >>> isErrorResult $ parse lower "ABC"
-- True
--
lower :: Parser Char
lower = satisfy isLower

-- | Return a parser that produces an upper-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not upper-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isUpper@ functions.
--
-- >>> parse upper "ABC"
-- Result >BC< 'A'
--
-- >>> isErrorResult $ parse upper Nil
-- True
--
-- >>> isErrorResult $ parse upper "abc"
-- True
--
upper :: Parser Char
upper = satisfy isUpper

-- | Return a parser that produces an alpha character but fails if
--
--   * The input is empty.
--
--   * The produced character is not alpha.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isAlpha@ functions.
--
-- >>> parse alpha "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult $ parse alpha Nil
-- True
--
-- >>> isErrorResult $ parse alpha "?bc"
-- True
--
alpha :: Parser Char
alpha = satisfy isAlpha

-- | Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
--
-- /Tip:/ Use @(=<<)@ and @pure@.
-- /Tip:/ Optionally use @List#foldRight@. If not, an explicit recursive call.
--
-- >>> parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef"
-- Result >def< "axC"
--
-- >>> isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef")
-- True
--
sequenceParser :: List (Parser a)
               -> Parser (List a)
sequenceParser = sequence

-- explicit recursive with (>>=) and `pure`
-- sequenceParser Nil       = pure Nil
-- -- sequenceParser (p :. ps) = p >>= (\a -> sequenceParser ps >>= (\as -> pure $ a :. as))
-- sequenceParser (p :. ps) = do
--   a <- p
--   as <- sequenceParser ps
--   pure $ a :. as

-- foldRight with (>>=) and `pure`
-- sequenceParser = foldRight f (pure Nil)
--   where f :: Parser a -> Parser (List a) -> Parser (List a)
--         -- Applicative version work because (<*>) using (>>=)
--         -- f pa pla = (:.) <$> pa <*> pla
--
--         -- so does Monad version
--         -- f pa pla = pa >>= (\a -> pla >>= (\as -> pure $ a :. as))
--         f pa pla = do
--           a <- pa
--           as <- pla
--           pure $ a :. as

-- | Return a parser that produces the given number of values off the given parser.
-- This parser fails if the given parser fails in the attempt to produce the given number of values.
--
-- /Tip:/ Use @sequenceParser@ and @List.replicate@.
--
-- >>> parse (thisMany 4 upper) "ABCDef"
-- Result >ef< "ABCD"
--
-- >>> isErrorResult (parse (thisMany 4 upper) "ABcDef")
-- True
--
thisMany :: Int
         -> Parser a
         -> Parser (List a)
-- thisMany n pa = sequenceParser (replicate n pa)
-- thisMany n pa = sequenceParser $ replicate n pa
-- thisMany n pa = (sequenceParser . replicate n) pa
-- thisMany n = sequenceParser . replicate n
-- thisMany n = (.) sequenceParser $ replicate $ n
thisMany = (.) sequenceParser . replicate

-- | This one is done for you.
--
-- /Age: positive integer/
--
-- >>> parse ageParser "120"
-- Result >< 120
--
-- >>> isErrorResult (parse ageParser "abc")
-- True
--
-- >>> isErrorResult (parse ageParser "-120")
-- True
--
ageParser :: Parser Int
-- ageParser =
--   (\k -> case read k of Empty  -> constantParser (UnexpectedString k)
--                         Full h -> pure h) =<< (list1 digit)

-- but my version is more readable ;)
ageParser = list1 digit >>= f
  where f k = case read k of
                Empty  -> unexpectedStringParser k
                Full h -> pure h

-- | Write a parser for Person.firstName.
-- /First Name: non-empty string that starts with a capital letter and is followed by zero or more lower-case letters/
--
-- /Tip:/ Use @(=<<)@, @pure@, @upper@, @list@ and @lower@.
--
-- >>> parse firstNameParser "Abc"
-- Result >< "Abc"
--
-- >>> isErrorResult (parse firstNameParser "abc")
-- True
--
firstNameParser :: Parser Chars

-- Monadic style
-- firstNameParser = upper >>= (\x -> list1 lower >>= (\xs -> pure $ x :. xs))
-- firstNameParser = do
--   x <- upper
--   xs <- list1 lower
--   pure $ x :. xs

-- Applicative style
-- firstNameParser = (++) <$> thisMany 1 upper <*> list1 lower
-- firstNameParser = lift2 (++) (thisMany 1 upper) (list1 lower)
firstNameParser = lift2 (:.) upper (list1 lower)


-- | Write a parser for Person.surname.
--
-- /Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters./
--
-- /Tip:/ Use @(=<<)@, @pure@, @upper@, @thisMany@, @lower@ and @list@.
--
-- >>> parse surnameParser "Abcdef"
-- Result >< "Abcdef"
--
-- >>> parse surnameParser "Abcdefghijklmnopqrstuvwxyz"
-- Result >< "Abcdefghijklmnopqrstuvwxyz"
--
-- >>> isErrorResult (parse surnameParser "Abc")
-- True
--
-- >>> isErrorResult (parse surnameParser "abc")
-- True
--
-- FIXME: CPU intensive and very slow (18s) on test case #2 "Abcdefghijklmnopqrstuvwxyz"
surnameParser :: Parser Chars
-- Monadic style
-- surnameParser = upper >>= (\c -> thisMany 5 lower >>= (\l5 -> list lower >>= (\ls -> pure (c :. l5 ++ ls))))
surnameParser = do
  c <- upper
  l5 <- thisMany 5 lower
  ls <- list lower
  pure (c :. (l5 ++ ls))

-- Applicative style
-- surnameParser = lift2 (:.) upper $ lift2 (++) (thisMany 5 lower) (list lower)

-- | Write a parser for Person.smoker.
--
-- /Smoker: character that must be @'y'@ or @'n'@/
--
-- /Tip:/ Use @is@ and @(|||)@./
--
-- >>> parse smokerParser "yabc"
-- Result >abc< True
--
-- >>> parse smokerParser "nabc"
-- Result >abc< False
--
-- >>> isErrorResult (parse smokerParser "abc")
-- True
--
smokerParser :: Parser Bool
-- smokerParser = do
--   c <- character
--   -- if c `elem` "yn"
--   -- then pure $ c == 'y'
--   -- else unexpectedCharParser c
--   case c of
--     'y' -> pure True
--     'n' -> pure False
--     _   -> unexpectedCharParser c

-- smokerParser = (is 'y' >>= const (valueParser True)) ||| (is 'n' >>= const (valueParser False))
smokerParser = (is 'y' >>= const ( pure True)) ||| (is 'n' >>= const ( pure False))

-- | Write part of a parser for Person#phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
--
-- /Phone: string of digits, dots or hyphens .../
--
-- /Tip:/ Use @list@, @digit@, @(|||)@ and @is@.
--
-- >>> parse phoneBodyParser "123-456"
-- Result >< "123-456"
--
-- >>> parse phoneBodyParser "123-4a56"
-- Result >a56< "123-4"
--
-- >>> isErrorResult $ parse phoneBodyParser "a123-456"
-- True
--
phoneBodyParser :: Parser Chars
-- phoneBodyParser = list1 (digit ||| is '.' ||| is '-')
phoneBodyParser = list1 $ digit ||| is '.' ||| is '-'

-- | Write a parser for Person.phone.
--
-- /Phone: ... but must start with a digit and end with a hash (#)./
--
-- /Tip:/ Use @(=<<)@, @pure@, @digit@, @phoneBodyParser@ and @is@.
--
-- >>> parse phoneParser "123-456#"
-- Result >< "123-456"
--
-- >>> parse phoneParser "123-456#abc"
-- Result >abc< "123-456"
--
-- >>> isErrorResult (parse phoneParser "123-456")
-- True
--
-- >>> isErrorResult (parse phoneParser "a123-456")
-- True
--
phoneParser :: Parser Chars
-- phoneParser = digit >>= (\d -> phoneBodyParser >>= (\ds -> is '#' >>= (\_ -> pure $ d :. ds)))
-- phoneParser = digit >>= (\d -> phoneBodyParser >>= (\ds -> is '#' >> pure (d :. ds)))
phoneParser = do
  d <- digit
  ds <- phoneBodyParser
  is '#'
  pure $ d :. ds

-- | Write a parser for Person.
--
-- /Tip:/ Use @(=<<)@,
--            @pure@,
--            @(>>>)@,
--            @spaces1@,
--            @ageParser@,
--            @firstNameParser@,
--            @surnameParser@,
--            @smokerParser@,
--            @phoneParser@.
--
-- >>> isErrorResult (parse personParser "")
-- True
--
-- >>> isErrorResult (parse personParser "12x Fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Cla y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 1x3-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y -123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 123-456.789")
-- True
--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789#"
-- Result >< Person 123 "Fred" "Clarkson" True "123-456.789"

--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789# rest"
-- Result > rest< Person 123 "Fred" "Clarkson" True "123-456.789"

--
-- >>> parse personParser "123  Fred   Clarkson    y     123-456.789#"
-- Result >< Person 123 "Fred" "Clarkson" True "123-456.789"
--
personParser :: Parser Person
-- personParser = ageParser >>=~ (\a ->
--                firstNameParser >>=~ (\b ->
--                surnameParser >>=~ (\c ->
--                smokerParser >>=~ (\d ->
--                phoneParser >>= (\e ->
--                pure (Person a b c d e))))))

-- -- -- NOTE: hlint suggested this (avoiding lambda \e ->)
-- personParser = ageParser >>=~ (\a ->
--                firstNameParser >>=~ (\b ->
--                surnameParser >>=~ (\c ->
--                smokerParser >>=~ (\d ->
--                phoneParser >>=
--                pure . Person a b c d))))

-- personParser = do
--   a <- ageParser
--   spaces1
--   b <- firstNameParser
--   spaces1
--   c <- surnameParser
--   spaces1
--   d <- smokerParser
--   spaces1
--   e <- phoneParser
--   pure $ Person a b c d e

-- -- NOTE: hlint suggested this (using (<$>))
-- personParser = do
--   a <- ageParser
--   spaces1
--   b <- firstNameParser
--   spaces1
--   c <- surnameParser
--   spaces1
--   d <- smokerParser
--   spaces1
--   Person a b c d <$> phoneParser

-- Applicative version
personParser = Person
             <$> ageParser
             <*>~ firstNameParser
             <*>~ surnameParser
             <*>~ smokerParser
             <*>~ phoneParser

-- Make sure all the tests pass!

----

-- Did you repeat yourself in `personParser` ? This might help:

(>>=~) :: Parser a
       -> (a -> Parser b)
       -> Parser b
(>>=~) p f = (p <* spaces1) >>= f

infixl 1 >>=~

-- or maybe this

(<*>~) :: Parser (a -> b)
       -> Parser a
       -> Parser b
(<*>~) f a = f <*> spaces1 *> a

infixl 4 <*>~
