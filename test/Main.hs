{-# LANGUAGE DeriveGeneric        #-}

module Main where

------------------------------------------------------------------------------
import           Data.Time
import           GHC.Generics
import           System.Random
import           Test.Hspec
------------------------------------------------------------------------------
import           Fake.Class
import           Fake.Types
import           Fake.Cover
------------------------------------------------------------------------------

testFake :: FGen a -> a
testFake (MkFGen f) = f $ mkStdGen 5

tc :: [FGen a] -> [a]
tc = testFake . sequence

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
      --it "Person" $
      --  tc gcover `shouldBe`
      --  [ Person "Alice" "Baker" (fromGregorian 2017 05 13) Nothing
      --  , Person "Frank" "Adams" (fromGregorian 2017 02 20) (Just "123-45-6789")
      --  ]

instance Cover Int where
    cover = [fakeEnumFromTo 0 100]

instance Cover Char where
    cover = [fakeEnumFromTo 'a' 'z']

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
