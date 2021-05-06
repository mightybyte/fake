{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

------------------------------------------------------------------------------
import           Control.Lens hiding (elements)
import           Data.Foldable
import           Data.Time
import           GHC.Generics
import           System.Random
import           Test.Hspec
import           Data.Text (unpack)
------------------------------------------------------------------------------
import           Fake.Cover
import           Fake.Combinators
import           Fake.Provider.Lang
import           Fake.Provider.Person.EN_US
import           Fake.Types
------------------------------------------------------------------------------

instance Cover Int where
    cover = Coverage [fakeInt 0 100]

instance Cover Char where
    cover = Coverage [fakeLetter]

data ThreePhonetic = Alpha | Bravo | Charlie
  deriving (Eq,Ord,Show,Generic)

instance Cover ThreePhonetic where
    cover = gcover

data Four = MOne Int
          | MTwo Char
          | MThree Int
          | MFour Char
  deriving (Eq,Ord,Show,Generic)

makePrisms ''Four

instance Cover Four where
    cover = gcover
      -- Use gcover for the majority of the generation, then tweak one field to
      -- be a different distribution.
      &>>= _MFour %%~ (\_ -> fakeEnumFromTo 'A' 'F')

birthdayCoverage :: Coverage Day
birthdayCoverage = fromGregorian
    <$> Coverage [fakeEnumFromTo 1950 2017]
    <*> Coverage [fakeInt 1 12]
    <*> Coverage [fakeInt 1 31]

ssnCoverage :: Coverage String
ssnCoverage = Coverage [elements ["123-45-6789", "000-00-0000"]]

data Person = Person
  { personFirstName :: String
  , personLastName  :: String
  , personBirthdate :: Day
  , personSSN       :: Maybe String
  } deriving (Eq,Ord,Show,Generic)

------------------------------------------------------------------------------
-- | Must be able to define a Cover instnance for Person without needing to
-- define instances for Day and String.
--instance Cover Person where
--    cover = gcover

instance Cover Person where
  cover = Person
    <$> fmap unpack (Coverage [unSingleWord <$> firstName])
    <*> fmap unpack (Coverage [unSingleWord <$> lastName])
    <*> birthdayCoverage
    <*> asum [ pure Nothing, Just <$> ssnCoverage ]


testFake :: FGen a -> a
testFake (MkFGen f) = f $ mkStdGen 5

tc :: Coverage a -> [a]
tc = testFake . sequence . unCoverage

------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
    describe "Fake.Cover" $ do

#if MIN_VERSION_random(1,2,0)
      it "Maybe Int" $
        tc gcover `shouldBe` [Nothing, Just (37 :: Int)]
      it "Either Int Char" $
        tc gcover `shouldBe` [Left (8 :: Int), Right 'f']
      it "(Maybe Int, ThreePhonetic)" $
        tc gcover `shouldBe`
        [(Nothing,Alpha),(Just (26 :: Int),Bravo),(Nothing,Charlie)]
      it "(Either ThreePhonetic Four)" $
        tc gcover `shouldBe`
        [ Left Alpha
        , Left Bravo
        , Left Charlie
        , Right (MOne 39)
        , Right (MTwo 'u')
        , Right (MThree 17)
        , Right (MFour 'C')
        ]
      -- Since Person contains one Maybe field, cover should generate two values
      it "Person" $
        tc cover `shouldBe`
        [ Person "Jaylen" "Massey" (fromGregorian 1967 1 21) Nothing
        , Person "Timothy" "Garcia" (fromGregorian 2007 4 18) (Just "000-00-0000")
        ]
#else
      it "Maybe Int" $
        tc gcover `shouldBe` [Nothing, Just (94 :: Int)]
      it "Either Int Char" $
        tc gcover `shouldBe` [Left (53 :: Int), Right 't']
      it "(Maybe Int, ThreePhonetic)" $
        tc gcover `shouldBe`
        [(Nothing,Alpha),(Just (84 :: Int),Bravo),(Nothing,Charlie)]
      it "(Either ThreePhonetic Four)" $
        tc gcover `shouldBe`
        [ Left Alpha
        , Left Bravo
        , Left Charlie
        , Right (MOne 32)
        , Right (MTwo 'j')
        , Right (MThree 17)
        , Right (MFour 'B')
        ]
      -- Since Person contains one Maybe field, cover should generate two values
      it "Person" $
        tc cover `shouldBe`
        [ Person "Opal" "Clark" (fromGregorian 1958 10 12) Nothing
        , Person "Katherine" "Oneill" (fromGregorian 1966 07 21) (Just "123-45-6789")
        ]
#endif
