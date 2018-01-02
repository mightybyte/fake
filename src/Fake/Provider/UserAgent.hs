{-# LANGUAGE ScopedTypeVariables #-}

module Fake.Provider.UserAgent where

------------------------------------------------------------------------------
import Data.Monoid
import Data.Time
import Text.Printf
------------------------------------------------------------------------------
import Fake.Class
import Fake.Combinators
import Fake.Provider.DateTime
import Fake.Provider.Locale
import Fake.Types
------------------------------------------------------------------------------

userAgent :: FGen String
userAgent = oneof
    -- TODO Might want to use a more realistic browser distribution
    [ chromeUserAgent
    , internetExplorerUserAgent
    , firefoxUserAgent
    , safariUserAgent
    , operaUserAgent
    ]

chromeUserAgent :: FGen String
chromeUserAgent = do
    plat <- fakePlatform
    appleA <- fakeInt 531 536
    appleB <- fakeInt 0 2
    let saf = show appleA <> "." <> show appleB
    chromeMajor <- fakeInt 13 15
    chromeMinor <- fakeInt 800 899
    let rest = printf
          "(%s) AppleWebKit/%s (KHTML, like Gecko) Chrome/%d.0.%d.0 Safari/%s"
          plat saf chromeMajor chromeMinor saf
    return $ "Mozilla/5.0 " <> rest

internetExplorerUserAgent :: FGen String
internetExplorerUserAgent = do
    plat <- windowsPlatform
    a <- fakeInt 5 9
    b <- fakeInt 3 5
    c <- fakeInt 0 1
    return $ printf "Mozilla/5.0 (compatible; MSIE %d.0; %s; Trident/%d.%d)"
                    a plat b c

underscoreToDash :: Char -> Char
underscoreToDash '_' = '-'
underscoreToDash c = c

dayStr :: Day -> String
dayStr = formatTime defaultTimeLocale "%Y%m%d"

firefoxUserAgent :: FGen String
firefoxUserAgent = do
    let a :: FGen String
        a = do
          d <- dayStr <$> dayBetween (fromGregorian 2011 1 1) (fromGregorian 2017 1 1)
          n <- fakeInt 4 15
          return $ printf "Gecko/%s Firefox/%d.0" d n
        b :: FGen String
        b = do
          d <- dayStr <$> dayBetween (fromGregorian 2010 1 1) (fromGregorian 2017 1 1)
          n <- fakeInt 1 20
          return $ printf "Gecko/%s Firefox/3.6.%d" d n
        c :: FGen String
        c = do
          d <- dayStr <$> dayBetween (fromGregorian 2010 1 1) (fromGregorian 2017 1 1)
          return $ printf "Gecko/%s Firefox/3.8" d
    ver <- oneof [a, b, c]

    let win :: FGen String
        win = do
          plat <- windowsPlatform
          l <- fmap underscoreToDash <$> fakeLocale
          n <- fakeInt 0 2
          return $ printf "(%s; %s; rv:1.9.%d.20) %s"
                          plat l n ver
        lin :: FGen String
        lin = do
          plat <- linuxPlatform
          n <- fakeInt 5 7
          return $ printf "(%s; rv:1.9.%d.20) %s" plat n ver

        mac :: FGen String
        mac = do
          plat <- macPlatform
          n <- fakeInt 2 6
          return $ printf "(%s; rv:1.9.%d.20) %s" plat n
    plat <- oneof [win, lin, mac]
    return $ "Mozilla/5.0 " <> plat


safariUserAgent :: FGen String
safariUserAgent = do
    saf :: String <- do
        a <- fakeInt 531 535
        b <- fakeInt 1 50
        c <- fakeInt 1 7
        return $ printf "%d.%d.%d" a b c
    twoVers <- elements [False, True]
    ver :: String <- if twoVers
             then printf "%d.%d" <$> fakeInt 4 5 <*> fakeInt 0 1
             else printf "%d.0.%d" <$> fakeInt 4 5 <*> fakeInt 1 5

    let win :: FGen String
        win = do
          plat <- windowsPlatform
          return $ printf "(Windows; U; %s) AppleWebKit/%s (KHTML, like Gecko) Version/%s Safari/%s"
                          plat saf ver saf
        mac :: FGen String
        mac = do
          plat <- macPlatform
          n <- fakeInt 2 6
          l <- fmap underscoreToDash <$> fakeLocale
          return $ printf "(%s rv:%d.0; %s) AppleWebKit/%s (KHTML, like Gecko) Version/%s Safari/%s"
                          plat n l saf ver saf
        ipod :: FGen String
        ipod = do
          a <- fakeInt 3 4
          b <- fakeInt 0 3
          c <- fakeInt 3 4
          d <- fakeInt 111 119
          l <- fmap underscoreToDash <$> fakeLocale
          return $ printf "(iPod; U; CPU iPhone OS %d_%d like Mac OS X; %s) AppleWebKit/%s (KHTML, like Gecko) Version/%d.0.5 Mobile/8B%d Safari/6%s"
                          a b l saf c d saf

    plat <- oneof [win, mac, ipod]
    return $ "Mozilla/5.0 " <> plat

operaUserAgent :: FGen String
operaUserAgent = do
    let getPlat = do
          useLinux <- elements [False, True]
          if useLinux then linuxPlatform else windowsPlatform
    let plat :: FGen String = printf "(%s; %s) Presto/2.9.%d Version/%d.00"
          <$> getPlat
          <*> fmap (fmap underscoreToDash) fakeLocale
          <*> fakeInt 160 190
          <*> fakeInt 10 12

    printf "Opera/%d.%d.%s"
      <$> fakeInt 8 9
      <*> fakeInt 10 99
      <*> plat

fakePlatform :: FGen String
fakePlatform = oneof [windowsPlatform, linuxPlatform, macPlatform]

windowsPlatform :: FGen String
windowsPlatform = elements
  [ "Windows 95", "Windows 98", "Windows 98; Win 9x 4.90", "Windows CE"
  , "Windows NT 4.0", "Windows NT 5.0", "Windows NT 5.01"
  , "Windows NT 5.1", "Windows NT 5.2", "Windows NT 6.0", "Windows NT 6.1"
  , "Windows NT 6.2"
  ]

linuxPlatform :: FGen String
linuxPlatform = do
    processor <- linuxProcessor
    return $ "X11; Linux " <> processor

macPlatform :: FGen String
macPlatform = do
    processor <- macProcessor
    b <- fakeInt 5 8
    c <- fakeInt 0 9
    return $ printf "Macintosh; %s Mac OS X 10_%d_%d" processor b c

linuxProcessor :: FGen String
linuxProcessor = elements ["i686", "x86_64"]

macProcessor :: FGen String
macProcessor = elements ["Intel", "PPC", "U; Intel", "U; PPC"]
