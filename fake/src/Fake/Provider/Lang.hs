{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fake.Provider.Lang
  ( SingleWord(..)
  , capitalize
  , lowerize
  , Phrase
  , phrase
  , phraseText
  ) where

import           Data.Bifunctor
#if MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Fake.Types

------------------------------------------------------------------------------
-- | Newtype wrapper representing a single word.
newtype SingleWord = SingleWord { unSingleWord :: Text }
#if MIN_VERSION_base(4,11,0)
  deriving (Eq,Ord,Show,Semigroup,Monoid,IsString)
#else
  deriving (Eq,Ord,Show,Monoid,IsString)
#endif

capitalize :: SingleWord -> SingleWord
capitalize = SingleWord .
             uncurry mappend . first T.toUpper . T.splitAt 1 . unSingleWord

lowerize :: SingleWord -> SingleWord
lowerize = SingleWord .
           uncurry mappend . first T.toLower . T.splitAt 1 . unSingleWord

------------------------------------------------------------------------------
newtype Phrase = Phrase { unPhrase :: [SingleWord] }
#if MIN_VERSION_base(4,11,0)
  deriving (Eq,Ord,Show,Semigroup,Monoid)
#else
  deriving (Eq,Ord,Show,Monoid)
#endif

phraseText :: Phrase -> Text
phraseText = T.unwords . map unSingleWord . unPhrase

phrase :: [FGen SingleWord] -> FGen Phrase
phrase = fmap Phrase . sequence

