module Fake.Types where

import Control.Monad
import System.Random

newtype FGen a = MkFGen { unFGen :: StdGen -> a }

instance Functor FGen where
  fmap f (MkFGen h) =
    MkFGen (\r -> f (h r))

instance Applicative FGen where
  pure  = return
  (<*>) = ap

instance Monad FGen where
  return x =
    MkFGen (\_ -> x)

  MkFGen m >>= k =
    MkFGen (\r ->
      let (r1,r2)  = split r
          MkFGen m' = k (m r1)
       in m' r2
    )

-- | Generates a random element in the given inclusive range.
fromRange :: Random a => (a,a) -> FGen a
fromRange rng = MkFGen (\r -> let (x,_) = randomR rng r in x)

-- | Generates a random element over the natural range of `a`.
pickAny :: Random a => FGen a
pickAny = MkFGen (\r -> let (x,_) = random r in x)

-- | Run a generator. The size passed to the generator is always 30;
-- if you want another size then you should explicitly use 'resize'.
generate :: FGen a -> IO a
generate (MkFGen g) = do
    r <- newStdGen
    return (g r)

