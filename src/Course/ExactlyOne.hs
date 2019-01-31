{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.ExactlyOne where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

data ExactlyOne a = ExactlyOne a deriving (Eq, Show)

runExactlyOne ::
  ExactlyOne a
  -> a
runExactlyOne (ExactlyOne a) = a

mapExactlyOne ::
  (a -> b)
  -> ExactlyOne a
  -> ExactlyOne b
mapExactlyOne f (ExactlyOne a) = ExactlyOne (f a)

bindExactlyOne ::
  (a -> ExactlyOne b)
  -> ExactlyOne a
  -> ExactlyOne b
bindExactlyOne f (ExactlyOne a) = f a

instance P.Functor ExactlyOne where
  fmap ::
    (a -> b)
    -> ExactlyOne a
    -> ExactlyOne b
  fmap = M.liftM

instance A.Applicative ExactlyOne where
  pure ::
    a
    -> ExactlyOne a
  pure = ExactlyOne

  (<*>) ::
    ExactlyOne (a -> b)
    -> ExactlyOne a
    -> ExactlyOne b
  (<*>) = M.ap

instance P.Monad ExactlyOne where
  return ::
    a
    -> ExactlyOne a
  return = ExactlyOne

  (>>=) ::
    ExactlyOne a
    -> (a -> ExactlyOne b)
    -> ExactlyOne b
  (>>=) =
    flip bindExactlyOne

