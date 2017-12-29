{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fake.Lang
  ( SingleWord(..)
  , capitalize
  , lowerize
  , Phrase
  , phrase
  , phraseText
  ) where

import           Data.Bifunctor
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Fake.Types

------------------------------------------------------------------------------
-- | Newtype wrapper representing a single word.
newtype SingleWord = SingleWord { unSingleWord :: Text }
  deriving (Eq,Ord,Show,Monoid,IsString)

capitalize :: SingleWord -> SingleWord
capitalize = SingleWord .
             uncurry mappend . first T.toUpper . T.splitAt 1 . unSingleWord

lowerize :: SingleWord -> SingleWord
lowerize = SingleWord .
           uncurry mappend . first T.toLower . T.splitAt 1 . unSingleWord

------------------------------------------------------------------------------
newtype Phrase = Phrase { unPhrase :: [SingleWord] }
  deriving (Eq,Ord,Show,Monoid)

phraseText :: Phrase -> Text
phraseText = T.unwords . map unSingleWord . unPhrase

phrase :: [FGen SingleWord] -> FGen Phrase
phrase = fmap Phrase . sequence

