module Fake.Class where

------------------------------------------------------------------------------
import Control.Monad
import Data.List
import Fake.Combinators
import Fake.Types
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Random generation of fake values.
class Fake a where
    -- | A generator for values of the given type.
    fake :: FGen a

instance Fake () where
    fake = return ()

instance Fake Bool where
    fake = fromRange (False,True)

instance Fake Ordering where
    fake = element [LT, EQ, GT]

instance Fake a => Fake (Maybe a) where
    fake = frequency [(1, return Nothing), (3, liftM Just fake)]

instance (Fake a, Fake b) => Fake (Either a b) where
    fake = oneof [liftM Left fake, liftM Right fake]

instance (Fake a, Fake b) => Fake (a,b) where
    fake = liftM2 (,) fake fake

instance (Fake a, Fake b, Fake c) => Fake (a,b,c) where
    fake = liftM3 (,,) fake fake fake

instance (Fake a, Fake b, Fake c, Fake d) => Fake (a,b,c,d) where
    fake = liftM4 (,,,) fake fake fake fake

instance (Fake a, Fake b, Fake c, Fake d, Fake e) => Fake (a,b,c,d,e) where
    fake = liftM5 (,,,,) fake fake fake fake fake

instance (Fake a, Fake b, Fake c, Fake d, Fake e, Fake f)
      => Fake (a,b,c,d,e,f) where
    fake = return (,,,,,)
          <*> fake <*> fake <*> fake <*> fake <*> fake <*> fake

instance (Fake a, Fake b, Fake c, Fake d, Fake e, Fake f, Fake g)
      => Fake (a,b,c,d,e,f,g) where
    fake = return (,,,,,,)
          <*> fake <*> fake <*> fake <*> fake <*> fake <*> fake <*> fake

-- GHC only has Generic instances up to 7-tuples

-- | Generates a list of a given length.
fakeVector :: Fake a => Int -> FGen [a]
fakeVector k = vectorOf k fake

-- | Generates an ordered list.
orderedList :: (Ord a, Fake a) => Int -> FGen [a]
orderedList n = sort <$> listUpTo n fake

-- | Generate an infinite list.
infiniteList :: Fake a => FGen [a]
infiniteList = infiniteListOf fake

fakeEnumFromTo :: Enum a => a -> a -> FGen a
fakeEnumFromTo from to =
    toEnum <$> fromRange (fromEnum from, fromEnum to)

fakeEnum :: (Enum a, Bounded a) => FGen a
fakeEnum = fakeEnumFromTo minBound maxBound
