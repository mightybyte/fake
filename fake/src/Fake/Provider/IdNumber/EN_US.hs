module Fake.Provider.IdNumber.EN_US
  ( ssn
  ) where

------------------------------------------------------------------------------
import Text.Printf
------------------------------------------------------------------------------
import Fake
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Generates a fake US social security number.  The have the form
-- area-group-serial.
ssn :: FGen String
ssn = do
    area <- ssnArea
    group <- ssnGroup
    serial <- ssnSerial
    return $ printf "%03d-%02d-%04d" area group serial

fixupArea :: Int -> Int
fixupArea 666 = 667
fixupArea n = n

ssnArea :: FGen Int
ssnArea = fixupArea <$> fakeInt 0 899

ssnGroup :: FGen Int
ssnGroup = fakeInt 1 99

ssnSerial :: FGen Int
ssnSerial = fakeInt 1 9999
