{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.Now.Spec (propsForInterpreter) where

import Imports
import Polysemy
import Polysemy.Check
import Polysemy.Input
import qualified Spar.Sem.Now as E
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

propsForInterpreter ::
  PropConstraints r f =>
  String ->
  (forall a. Sem r a -> IO (f a)) ->
  Spec
propsForInterpreter interpreter lower = do
  describe interpreter $ do
    prop "now/now" $ prop_nowNow Nothing lower

-- | All the constraints we need to generalize properties in this module.
-- A regular type synonym doesn't work due to dreaded impredicative
-- polymorphism.
class
  (Functor f, Member E.Now r, Member (Input ()) r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z)) =>
  PropConstraints r f

instance
  (Functor f, Member E.Now r, Member (Input ()) r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z)) =>
  PropConstraints r f

prop_nowNow ::
  PropConstraints r f =>
  Maybe (f Bool -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_nowNow =
  -- NOTE: This @Input ()@ effect is a workaround to an oversight in
  -- @polysemy-check@. 'prepropLaw' wants to synthesize some actions to run
  -- before and after its generators, and check their results for equality. We
  -- can't use 'Now' as this effect, because 'E.get' won't return equivalent
  -- results! And we can't keep it empty, because that triggers a crash in
  -- @polysemy-check@. Thus @Input ()@, which isn't beautiful, but works fine.
  prepropLaw @'[Input ()] $ do
    pure $
      simpleLaw
        (liftA2 (<=) E.get E.get)
        ( pure True
        )
