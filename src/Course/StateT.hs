{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) :: forall a b.
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<$>) ab (StateT rstSFA) = StateT { runStateT = rst }
    where rst :: s -> f (b, s)
          rst s = let fas = rstSFA s in (<$>) (ff ab) fas
          ff :: (a -> b) -> (a,s) -> (b,s)
          ff f (a,s) = (f a, s)

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure :: forall a.
    a
    -> StateT s f a
  pure a = StateT { runStateT = rst }
    where rst :: s -> f (a,s)
          rst s = pure (a,s)

  (<*>) ::
    StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  StateT rstfAB <*> StateT rstfA = StateT { runStateT = rst }
    where rst = (\(ab, t) -> ff ab <$> rstfA t) <=< rstfAB
          ff :: (a -> b) -> (a,s) -> (b,s)
          ff f (a,s) = (f a, s)

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  a2stfb =<< StateT rstfA = StateT { runStateT = rs }
    where rs s = rstfA s >>= \(a, t) -> runStateT (a2stfb a) t

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' f = StateT { runStateT = rst }
  where rst s = ExactlyOne (f s)

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' (StateT rs) = runExactlyOne . rs

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
--
-- >>> execT (StateT $ \s -> Full ((), s + 1)) 2
-- Full 3
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT (StateT rst) s = snd <$> rst s

-- | Run the `State'` seeded with `s` and retrieve the resulting state.
--
-- >>> exec' (state' $ \s -> ((), s + 1)) 2
-- 3
exec' ::
  State' s a
  -> s
  -> s
exec' st = runExactlyOne . execT st

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
--
-- >>> evalT (StateT $ \s -> Full (even s, s + 1)) 2
-- Full True
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT rst) s = fst <$> rst s

-- | Run the `State'` seeded with `s` and retrieve the resulting value.
--
-- >>> eval' (state' $ \s -> (even s, s + 1)) 5
-- False
eval' ::
  State' s a
  -> s
  -> a
eval' st = runExactlyOne . evalT st

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Applicative f =>
  StateT s f s
getT = StateT { runStateT = rst }
  where rst s = pure (s, s)

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Applicative f =>
  s
  -> StateT s f ()
putT s = StateT { runStateT = rst }
  where rst = const $ pure ((),s)

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' :: forall a.
  Ord a =>
  List a
  -> List a
distinct' xs = eval' st S.empty
  where st :: State' (S.Set a) (List a)
        st = filtering fp xs
        fp :: a -> State' (S.Set a) Bool
        fp a = state' (\s -> (S.notMember a s , S.insert a s))

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF :: forall a.
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF xs = evalT st S.empty
  where st :: StateT (S.Set a) Optional (List a)
        st = filtering fp xs
        fp :: a -> StateT (S.Set a) Optional Bool
        fp a = StateT (ps a)
        ps :: a -> S.Set a -> Optional (Bool, S.Set a)
        ps a s | a > 100 = Empty
               | otherwise = Full (S.notMember a s , S.insert a s)

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) :: forall a b.
    (a -> b)
    -> OptionalT f a
    -> OptionalT f b
  f <$> OptionalT rotA = OptionalT rotB
    where rotB :: f (Optional b)
          rotB = ((<$>) . (<$>)) f rotA

-- | Implement the `Applicative` instance for `OptionalT f` given a Monad f.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Monad f => Applicative (OptionalT f) where
  pure :: forall a.
    a
    -> OptionalT f a
  -- pure = OptionalT . pure .pure
  pure a = OptionalT { runOptionalT = rot }
    where rot :: f (Optional a)
          rot = pure (pure a)

  (<*>) :: forall a b.
    OptionalT f (a -> b)
    -> OptionalT f a
    -> OptionalT f b
  oab <*> oa = do
    ab <- oab
    a <- oa
    pure $ ab a

  -- TODO: find out why below versions didn't pass the tests
  -- TODO: use `onFull`
  --
  -- OptionalT fab <*> OptionalT fa = OptionalT { runOptionalT = rot }
  --   where rot :: f (Optional b)
  --         rot = do
  --           ab <- fab
  --           a <- fa
  --           pure $ ab <*> a
  --
  -- oab <*> oa = OptionalT (lift2 (<*>) (runOptionalT oab) (runOptionalT oa))
  -- OptionalT ab <*> OptionalT a = OptionalT { runOptionalT = rot }
  --   where rot :: f (Optional b)
  --         rot = lift2 (<*>) ab a


-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  (=<<) :: forall a b.
    (a -> OptionalT f b)
    -> OptionalT f a
    -> OptionalT f b
  f =<< OptionalT x = OptionalT { runOptionalT = rot }
    where rot :: f (Optional b)
          rot = x >>= z
          z Empty    = pure Empty
          z (Full a) = runOptionalT (f a)

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) ::
    (a -> b)
    -> Logger l a
    -> Logger l b
  f <$> Logger l a = Logger l (f a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure ::
    a
    -> Logger l a
  pure = Logger Nil

  (<*>) ::
    Logger l (a -> b)
    -> Logger l a
    -> Logger l b
  Logger la f <*> Logger lb a = Logger (la ++ lb) (f a)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) ::
    (a -> Logger l b)
    -> Logger l a
    -> Logger l b
  f =<< Logger l a = Logger (l ++ la)  b
    where Logger la b = f a

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 = (.) Logger pure

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG :: forall a.
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG xs = runOptionalT $ evalT (filtering f xs) S.empty
  where f :: a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
        f a = StateT { runStateT = rst }
          where rst s = OptionalT { runOptionalT = rot s }
                rot :: S.Set a -> Logger Chars (Optional (Bool, S.Set a))
                rot s | a > 100 = log1 abrt Empty
                      | even a  = log1 evn $ check s
                      | otherwise = pure $ check s
                check :: S.Set a -> Optional (Bool, S.Set a)
                check s = Full (S.notMember a s, S.insert a s)
                abrt = "aborting > 100: " ++ fromString (show a)
                evn = "even number: " ++ fromString (show a)

onFull ::
  Applicative f =>
  (t -> f (Optional a))
  -> Optional t
  -> f (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
