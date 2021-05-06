{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generate fake values with full constructor coverage
--
-- The idea behind Fake.Cover is that although exhaustive testing is highly
-- exponential, you can cover a large portion of the likely problem cases by
-- exercising all the constructors of a data type and associated fields. This
-- approach only requires a sub-exponential number of cases--far fewer than what
-- you need for the exhaustive approach. The number of test cases needed to
-- ensure that you have full coverage of all the constructors is given by the
-- following relations for product and sum types:
--
-- numCases (a, b) = max (numCases a) (numCases b)
--
-- numCases (Either a b) = numCases a + numCases b
--
-- See the test suite for examples of how many values are generated for
-- different data types.
module Fake.Cover
  ( gcover
  , Coverage(..)
  , Cover(..)
  , bindCover
  , (&>>=)
  ) where

------------------------------------------------------------------------------
import Control.Applicative
import GHC.Generics as G
------------------------------------------------------------------------------
import Fake.Types
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Coverage is a list of values, implemented here with a newtype around a
-- list of fake value generators.  It's @[FGen a]@ instead of @FGen [a]@
-- because we don't want to have to evaluate the 'FGen' monad to work with
-- coverage lists.
newtype Coverage a = Coverage { unCoverage :: [FGen a] }
  deriving (Functor)

instance Applicative Coverage where
  pure = Coverage . pure . pure
  Coverage as <*> Coverage bs = Coverage $ zipWith (<*>)
     (as ++ take (newlen - alen) (cycle as))
     (bs ++ take (newlen - blen) (cycle bs))
   where
    alen = length as
    blen = length bs
    newlen = max alen blen

------------------------------------------------------------------------------
-- | In some situations you don't have the ability to modify a data structure
-- and need to define different Cover instances for different fields that have
-- the same type.  In these situations, instead of implementing the gcover
-- logic by hand, you could alternatively use gcover to generate stock
-- coverage values and then go back and replace the necessary fields with more
-- appropriate generators.  This bind-like operation provides an easy way to
-- do that.
bindCover :: Coverage a -> (a -> FGen b) -> Coverage b
bindCover (Coverage gens) f = Coverage $ map (>>= f) gens

------------------------------------------------------------------------------
-- | Convenience infix operator for bindCover.
--
-- @
-- instance Cover Foo where
--   cover = gcover
--     &>>= fooField %%~ (\_ -> fakeFooField)
-- @
(&>>=) :: Coverage a -> (a -> FGen b) -> Coverage b
(&>>=) = bindCover
infixl 1 &>>=

instance Alternative Coverage where
  empty = Coverage empty
  Coverage as <|> Coverage bs = Coverage (as ++ bs)


------------------------------------------------------------------------------
-- | A type class that generates a list of values giving full construcor
-- coverage for data types.  You can write your own instances by hand or you
-- can use the default instance which calls 'gcover' provided your data type
-- has a Generic instance.
class Cover a where
    cover :: Coverage a
    default cover :: (Generic a, GCover ga, ga ~ G.Rep a) => Coverage a
    cover = gcover

instance Cover () where
    cover = gcover

instance Cover a => Cover (Maybe a) where
    cover = gcover

instance (Cover a, Cover b) => Cover (Either a b) where
    cover = gcover

instance (Cover a, Cover b) => Cover (a,b) where
    cover = gcover

instance (Cover a, Cover b, Cover c) => Cover (a,b,c) where
    cover = gcover

instance (Cover a, Cover b, Cover c, Cover d) => Cover (a,b,c,d) where
    cover = gcover

instance (Cover a, Cover b, Cover c, Cover d, Cover e)
      => Cover (a,b,c,d,e) where
    cover = gcover

instance (Cover a, Cover b, Cover c, Cover d, Cover e, Cover f)
      => Cover (a,b,c,d,e,f) where
    cover = gcover

instance (Cover a, Cover b, Cover c, Cover d, Cover e, Cover f, Cover g)
      => Cover (a,b,c,d,e,f,g) where
    cover = gcover
-- GHC only has Generic instances up to 7-tuples


------------------------------------------------------------------------------
-- | A generic function that gives you full constructor coverage for a data
-- type.  Using this function as the 'Cover' instance for a data type avoids
-- the need to explicitly enumerate values that include coverage of all
-- constructors.
gcover :: (Generic a, GCover ga, ga ~ G.Rep a) => Coverage a
gcover = Coverage $ fmap G.to <$> genericCover


------------------------------------------------------------------------------
-- | Used to implement 'gcover'.
class GCover a where
    genericCover :: [FGen (a x)]

instance GCover G.U1 where
    genericCover = pure $ pure G.U1

instance Cover c => GCover (G.K1 i c) where
    genericCover = fmap G.K1 <$> unCoverage cover

instance GCover f => GCover (G.M1 i c f) where
    genericCover = fmap G.M1 <$> genericCover

instance (GCover a, GCover b) => GCover (a G.:*: b) where
    genericCover = unCoverage $
      (G.:*:) <$> Coverage genericCover <*> Coverage genericCover

instance (GCover a, GCover b) => GCover (a G.:+: b) where
    genericCover = unCoverage $
      (G.L1 <$> Coverage genericCover) <|> (G.R1 <$> Coverage genericCover)
