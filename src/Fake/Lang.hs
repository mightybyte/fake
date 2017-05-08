{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fake.Lang
  ( Phrase
  , phrase
  , phraseText
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Fake.Types

newtype Phrase = Phrase { unPhrase :: [Text] }
  deriving (Eq,Ord,Show,Monoid)

phraseText :: Phrase -> Text
phraseText = T.unwords . unPhrase

phrase :: [FGen Text] -> FGen Phrase
phrase = fmap Phrase . sequence

