module Fake.Provider.DateTime where

------------------------------------------------------------------------------
import Data.Time
------------------------------------------------------------------------------
import Fake.Class
import Fake.Types
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Generates a random UTCTime in the range [from, to].
utcBetween :: UTCTime -> UTCTime -> FGen UTCTime
utcBetween from to = do
    delta <- fromRange (0 :: Double, realToFrac $ diffUTCTime to from)
    return $ addUTCTime (realToFrac delta) from

------------------------------------------------------------------------------
-- | Generates a random Day in the range [from, to].
dayBetween :: Day -> Day -> FGen Day
dayBetween from to = do
    delta <- fromRange (0, diffDays to from)
    return $ addDays delta from

dayBetweenYears :: Integer -> Integer -> FGen Day
dayBetweenYears ystart yend =
    fakeEnumFromTo (fromGregorian ystart 1 1) (fromGregorian yend 12 31)

timeBetweenHours :: Int -> Int -> FGen DiffTime
timeBetweenHours hstart hend = secondsToDiffTime <$> fromRange (fromIntegral from, fromIntegral to)
  where
    from = hstart * 3600
    to = hend * 3599

utcBetweenYears :: Integer -> Integer -> FGen UTCTime
utcBetweenYears ystart yend = UTCTime <$> dayBetweenYears ystart yend <*> timeBetweenHours 0 24
