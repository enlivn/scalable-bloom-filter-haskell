{-|
Module      : TestTypes
Description : Define types for testing.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestTypes where

import Data.ByteString as S (ByteString, pack)
import Data.ByteString.Lazy as L (ByteString, pack)
import System.Random (Random)
import Test.QuickCheck

runNTests :: Testable a => Int -> a -> IO ()
runNTests n = quickCheckWith (stdArgs{maxSuccess=n})

newtype NumElemsForNoScaling = NumElemsForNoScaling Int
    deriving (Random, Show, Num)

instance Arbitrary NumElemsForNoScaling where
    arbitrary = choose(1, 1024)

-- we need at least 1025 elements to force the scalable filter to
-- scale up (because the first constituent filter can accommodate
-- 1024 elements)
newtype NumElemsToForceScaling = NumElemsToForceScaling Int
    deriving (Random, Show, Num)

instance Arbitrary NumElemsToForceScaling where
    arbitrary = choose(1025, 10000)

-- limit the number of elements to 10000 so that tests don't
-- take too long
newtype LimitNumElems = LimitNumElems Int
    deriving (Random, Show, Num)

instance Arbitrary LimitNumElems where
    arbitrary = choose(1,10000)

newtype FalsePositiveRate = FalsePositiveRate Double
    deriving (Num, Fractional, Random, Show)

instance Arbitrary FalsePositiveRate where
    arbitrary = choose (eps, 1-eps)
        where eps = 1e-6

instance Arbitrary L.ByteString where
    arbitrary = fmap L.pack arbitrary

instance Arbitrary S.ByteString where
    arbitrary = fmap S.pack arbitrary
