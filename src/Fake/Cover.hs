{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generate fake values with full constructor coverage
--
-- The idea behind Fake.Cover is that although exhaustive testing is highly
-- exponential, you can cover a majority of the likely problem cases with a
-- sub-exponential amount of work.  The number of test cases needed to ensure
-- that you have full coverage of all the constructors is given by the
-- following relations for product and sum types:
--
-- numCases (a, b) = max (numCases a) (numCases b)
-- numCases (Either a b) = numCases a + numCases b
module Fake.Cover (Cover(..), gcover) where

------------------------------------------------------------------------------
import Control.Monad
import GHC.Generics as G
------------------------------------------------------------------------------
import Fake.Types
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A type class that generates a list of values giving full construcor
-- coverage for data types.  You can write your own instances by hand or you
-- can use @instance Cover where cover = gcover@.
class Cover a where
    cover :: [FGen a]

instance Cover () where
    cover = gcover

instance Cover a => Cover (Maybe a) where
    cover = gcover

instance (Cover a, Cover b) => Cover (Either a b) where
    cover = gcover

instance (Cover a, Cover b) => Cover (a,b) where
    cover = gcover

instance (Cover a, Cover b, Cover c) => Cover (a,b,c) where
    cover = gcover

instance (Cover a, Cover b, Cover c, Cover d) => Cover (a,b,c,d) where
    cover = gcover

instance (Cover a, Cover b, Cover c, Cover d, Cover e)
      => Cover (a,b,c,d,e) where
    cover = gcover

instance (Cover a, Cover b, Cover c, Cover d, Cover e, Cover f)
      => Cover (a,b,c,d,e,f) where
    cover = gcover

instance (Cover a, Cover b, Cover c, Cover d, Cover e, Cover f, Cover g)
      => Cover (a,b,c,d,e,f,g) where
    cover = gcover

-- Apparently these tuples don't have Generic instances :(

--instance ( Cover a, Cover b, Cover c, Cover d, Cover e, Cover f, Cover g
--         , Cover h)
--      => Cover (a,b,c,d,e,f,g,h) where
--    cover = gcover
--
--instance ( Cover a, Cover b, Cover c, Cover d, Cover e
--         , Cover f, Cover g, Cover h, Cover i
--         )
--      => Cover (a,b,c,d,e,f,g,h,i)
--  where
--    cover = gcover
--
--instance ( Cover a, Cover b, Cover c, Cover d, Cover e
--         , Cover f, Cover g, Cover h, Cover i, Cover j
--         )
--      => Cover (a,b,c,d,e,f,g,h,i,j)
--  where
--    cover = gcover


------------------------------------------------------------------------------
class GCover a where
    genericCover :: [FGen (a x)]

instance GCover G.U1 where
    genericCover = pure $ pure G.U1

instance Cover c => GCover (G.K1 i c) where
    genericCover = fmap G.K1 <$> cover

instance GCover f => GCover (G.M1 i c f) where
    genericCover = fmap G.M1 <$> genericCover

instance (GCover a, GCover b) => GCover (a G.:*: b) where
    genericCover = zipWith (liftM2 (G.:*:))
                     (acover ++ take (newlen - alen) (cycle acover))
                     (bcover ++ take (newlen - blen) (cycle bcover))
      where
        acover = genericCover :: [FGen (a x)]
        alen = length acover
        bcover = genericCover :: [FGen (b x)]
        blen = length bcover
        newlen = max alen blen

instance (GCover a, GCover b) => GCover (a G.:+: b) where
    genericCover = fmap (fmap G.L1) genericCover ++
                   fmap (fmap G.R1) genericCover

gcover :: (Generic a, GCover ga, ga ~ G.Rep a) => [FGen a]
gcover = fmap G.to <$> genericCover

