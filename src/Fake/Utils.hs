{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fake.Utils where

------------------------------------------------------------------------------
import           Data.String
import           Data.Text (Text)
import           GHC.Exts
------------------------------------------------------------------------------
import           Fake.Combinators
import           Fake.Types
import           Fake.Class
------------------------------------------------------------------------------

numerify :: Char -> FGen Char
numerify '#' = fakeDigit
numerify '%' = fakeDigitNonzero
-- TODO Original Python also includes empty string for ! and @.  We might
-- consider duplicating that behavior later, but for now this behavior will
-- work since it generates a strict subset of the things the original
-- generates.
numerify '!' = fakeDigit
numerify '@' = fakeDigitNonzero
numerify c = return c

lexify :: [Char] -> Char -> FGen Char
lexify letters '?' = elements letters
lexify _ c = return c

bothify :: [Char] -> Char -> FGen Char
bothify _ '#' = fakeDigit
bothify _ '%' = fakeDigitNonzero
bothify _ '!' = fakeDigit
bothify _ '@' = fakeDigitNonzero
bothify letters '?' = elements letters
bothify _ c = return c

newtype NumberScheme = NumberScheme { unNumberScheme :: Text }
  deriving (Eq,Ord,Read,Show,IsString)

fakeNumberScheme :: NumberScheme -> FGen Text
fakeNumberScheme (NumberScheme t) = fmap fromList $ mapM numerify $ toList t

newtype AlphaScheme = AlphaScheme { unAlphaScheme :: Text }
  deriving (Eq,Ord,Read,Show,IsString)

fakeAlphaScheme :: AlphaScheme -> FGen Text
fakeAlphaScheme (AlphaScheme t) =
    fmap fromList $ mapM (lexify ['a'..'z']) $ toList t

newtype AlphaNumScheme = AlphaNumScheme { unAlphaNumScheme :: Text }
  deriving (Eq,Ord,Read,Show,IsString)

fakeAlphaNumScheme :: AlphaNumScheme -> FGen Text
fakeAlphaNumScheme (AlphaNumScheme t) =
    fmap fromList $ mapM (bothify ['a'..'z']) $ toList t

------------------------------------------------------------------------------hc
-- | Generates two distinct values.
distinctPair :: (Fake a, Eq a) => FGen (a, a)
distinctPair =
  fake >>= \a -> do
        b <- suchThat fake (/= a)
        return (a,b)

------------------------------------------------------------------------------
-- | Tries to generate two distinct values. This is faster than
-- distinctPair since it only tries to generate the pair once!.
distinctPairMaybe :: (Fake a, Eq a) => FGen (Maybe (a, a))
distinctPairMaybe =
  fake >>= \a -> do
        mb <- suchThatMaybe fake (/= a)
        return $ case mb of
            Just b -> Just (a,b)
            Nothing -> Nothing
