{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric        #-}

module Main where

------------------------------------------------------------------------------
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

testFake :: FGen a -> a
testFake (MkFGen f) = f $ mkStdGen 5

tc :: Coverage a -> [a]
tc = testFake . sequence . unCoverage

------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
    describe "Fake.Cover" $ do
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
        , Right (MOne 96)
        , Right (MTwo 'k')
        , Right (MThree 12)
        , Right (MFour 'v')
        ]
      -- Since Person contains one Maybe field, cover should generate two values
      it "Person" $
        tc cover `shouldBe`
        [ Person "William" "Russell" (fromGregorian 1958 10 12) Nothing
        , Person "Michael" "Brooks" (fromGregorian 1966 07 21) (Just "123-45-6789")
        ]

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

instance Cover Four where
    cover = gcover

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
