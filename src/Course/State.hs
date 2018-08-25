{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a = State
  {
    runState :: s -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec :: State s a
     -> s
     -> s
exec st s = snd $ runState st s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval :: State s a
     -> s
     -> a
eval st s = fst $ runState st s

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
--
get :: State s s
get = State { runState = rs }
  where rs :: s -> (s,s)
        rs s = (s,s)
-- get = State { runState = \s -> (s,s) }
-- get = State (\s -> (s, s))

-- | A `State` where the resulting state is seeded with the given value and
-- `()` state.
--
-- >>> runState (put 1) undefined
-- ((),1)
--
put :: forall s. s
    -> State s ()
put s = State { runState = rs }
  where rs :: s -> ((), s)
        rs _ = ((), s)
-- put s = State { runState = \_ -> ((),s) }
--       = State (\_ -> ((), s))
--       = State (\_ -> (,) () s)
--       = State (const ((,) () s))
--       = State . const . (,) () $ s
-- put = State . const . (,) ()

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
--
instance Functor (State s) where
  (<$>) :: forall a b. (a -> b)
        -> State s a
        -> State s b
  -- (<$>) ab sta = State { runState = rs }
  --   where rs :: s -> (b, s)
  --         rs x = (,) ((ab . fst . rsax) x) ((snd . rsax) x)
  --         rsax :: s -> (a, s)
  --         rsax x = runState sta x
  --
  -- more readable when using inside runState of state a
  (<$>) f (State staRs) = State { runState = stbRs }
    where stbRs :: s -> (b,s)
          stbRs s = let (a, ps) = staRs s
                    in (f a, ps)
          -- stbRs s = (f a, ps)
          --   where (a, ps) = staRs s

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
--
instance Applicative (State s) where
  pure :: forall a. a
       -> State s a
  pure a = State { runState = rs }
    where rs :: s -> (a, s)
          rs s = (a , s)
  -- pure a = State (\s -> (a, s))

  (<*>) :: forall a b. State s (a -> b)
        -> State s a
        -> State s b
  (<*>) (State stfRs) (State staRs) = State { runState = stbRs }
    where stbRs :: s -> (b,s)
          -- 1. output state will get s to get (a -> b) and sf from first input
          -- 2. get (a, sa) from runState on 2nd input using previous state sf
          -- 3. appply (a -> b) on a to get b and return new state sa
          stbRs s = let (ab,sf) = stfRs s   -- 1.
                        (a, sa) = staRs sf  -- 2.
                    in (ab a, sa)           -- 3.
          -- where .. let .. in vs where .. where, which one is more readable ?
          -- stbRs s = (ab a, sa)                -- 3.
          --           where (ab,sf) = stfRs s   -- 1.
          --                 (a, sa) = staRs sf  -- 2.

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
--
instance Monad (State s) where
  (=<<) :: forall a b. (a -> State s b)
        -> State s a
        -> State s b
  (=<<) sf (State saRS) = State { runState = sbRs }
    where sbRs s = let (a, as) = saRS s
                   in runState (sf a) as
    -- where .. let .. in vs where .. where, which one is more readable ?
    -- where sbRs s = runState (sf a) as
    --                where (a, as) = saRS s

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
--
findM :: forall a f. Monad f
      => (a -> f Bool)
      -> List a
      -> f (Optional a)

-- use explicit recursion
-- findM _ Nil = pure Empty
-- findM mp (a :. as) = mp a >>= \p -> if p then pure (Full a) else findM mp as

-- explicit with `do` notation
-- findM _ Nil = pure Empty
-- findM mp (a :. as) = do
--   p <- mp a
--   if p then pure (Full a)
--        else findM mp as

-- use foldRight
-- findM mp = foldRight f (pure Empty)
--   where f :: a -> f (Optional a) -> f (Optional a)
--         f a foa = mp a >>= \p -> if p then pure (Full a) else foa

-- use foldRight w/ `do`
findM mp = foldRight f (pure Empty)
  where f :: a -> f (Optional a) -> f (Optional a)
        f a foa = do
          p <- mp a
          if p then pure (Full a)
               else foa

-- | Helper function to run State over a list with monadic function and Set's element cheking function
--
runList :: (Ord a, Functor f)
        => ((a -> State (S.Set a) Bool) -> List a -> State (S.Set a) (f a))  -- ^ findM, filtering
        -> (a -> S.Set a -> Bool)                                            -- ^ S.member, S.notMember
        -> List a                                                            -- ^ List to process
        -> f a                                                               -- ^ Optional a, List a
runList mf sf ls = eval (mf mp ls) S.empty
  where --mp :: a -> State (S.Set a) Bool
        mp a = State { runState = (rs a) }
        --rs :: a -> S.Set a -> (Bool, S.Set a)
        rs a s = (sf a s, S.insert a s)

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- >>> firstRepeat (0 :. 1 :. 0 :. 2 :. Nil)
-- Full 0
--
-- >>> firstRepeat (0 :. 1 :. 2 :. 3 :. Nil)
-- Empty
--
-- prop> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
--
firstRepeat :: Ord a
            => List a
            -> Optional a
-- firstRepeat ls = runList findM S.member ls
firstRepeat = runList findM S.member

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
--
distinct :: Ord a
         => List a
         -> List a
distinct = runList filtering S.notMember

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
--
isHappy :: Integer
        -> Bool
isHappy = contains 1 .  firstRepeat . gen
  where gen :: Integer -> List Integer
        gen = produce (toInteger . ssd . show')
        ssd :: List Char -> Int
        ssd = sum . map (square . digitToInt)

-- | square x = x * x
--
-- >>> square 3
-- 9
--
-- prop> \x -> square x == x * x
--
square :: Num a => a -> a
-- square a = join (*) $ a
square = join (*)

-- join (*) = (=<<) (*) id
--          = \t -> (*) (id  t) t
--          = \t -> (*) t t
