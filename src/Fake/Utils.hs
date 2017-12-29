{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fake.Utils where

------------------------------------------------------------------------------
import           Data.String
import           Data.Text (Text)
import           GHC.Exts
------------------------------------------------------------------------------
import           Fake.Class
import           Fake.Combinators
import           Fake.Types
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
lexify letters '?' = element letters
lexify _ c = return c

bothify :: [Char] -> Char -> FGen Char
bothify _ '#' = fakeDigit
bothify _ '%' = fakeDigitNonzero
bothify _ '!' = fakeDigit
bothify _ '@' = fakeDigitNonzero
bothify letters '?' = element letters
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
