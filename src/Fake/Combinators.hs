module Fake.Combinators where

------------------------------------------------------------------------------
import Control.Monad
import Data.List
import Data.Ord
import Fake.Types
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- ** Common generator combinators

-- | Generates a value that satisfies a predicate.
suchThat :: FGen a -> (a -> Bool) -> FGen a
gen `suchThat` p =
  do mx <- gen `suchThatMaybe` p
     case mx of
       Just x  -> return x
       Nothing -> gen `suchThat` p

-- | Tries to generate a value that satisfies a predicate.
suchThatMaybe :: FGen a -> (a -> Bool) -> FGen (Maybe a)
gen `suchThatMaybe` p = do
    x <- gen
    return $ if p x then Just x else Nothing

-- | Randomly uses one of the given generators. The input list
-- must be non-empty.
oneof :: [FGen a] -> FGen a
oneof [] = error "Fake.oneof used with empty list"
oneof gs = fromRange (0,length gs - 1) >>= (gs !!)

------------------------------------------------------------------------------
-- | Chooses one of the given generators, with a weighted random distribution.
-- The input list must be non-empty.
frequency :: [(Int, FGen a)] -> FGen a
frequency [] = error "Fake.frequency used with empty list"
frequency xs0 = fromRange (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "Fake.pick used with empty list"

-- | Generates one of the given values. The input list must be non-empty.
element :: [a] -> FGen a
element [] = error "Fake.element used with empty list"
element xs = (xs !!) `fmap` fromRange (0, length xs - 1)

-- | Generates a random subsequence of the given list.
sublistOf :: [a] -> FGen [a]
sublistOf = filterM (\_ -> fromRange (False, True))

-- | Generates a random permutation of the given list.
shuffle :: [a] -> FGen [a]
shuffle xs = do
  ns <- vectorOf (length xs) (fromRange (minBound :: Int, maxBound))
  return (map snd (sortBy (comparing fst) (zip ns xs)))

-- | Generates a list of random length.
listUpTo :: Int -> FGen a -> FGen [a]
listUpTo n gen = do
    k <- fromRange (0,n)
    vectorOf k gen

-- | Generates a non-empty list of random length. The maximum length
-- depends on the size parameter.
listUpTo1 :: Int -> FGen a -> FGen [a]
listUpTo1 n gen = do
    k <- fromRange (1,1 `max` n)
    vectorOf k gen

-- | Generates a list of the given length.
vectorOf :: Int -> FGen a -> FGen [a]
vectorOf = replicateM

-- | Generates an infinite list.
infiniteListOf :: FGen a -> FGen [a]
infiniteListOf gen = sequence (repeat gen)
