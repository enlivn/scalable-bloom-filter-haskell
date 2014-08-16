{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestTypes where

import Data.ByteString as S (ByteString, pack)
import Data.ByteString.Lazy as L (ByteString, pack)
import System.Random (Random)
import Test.QuickCheck

newtype NumElemsToInsert = NumElemsToInsert Int
    deriving (Num, Random, Show)

instance Arbitrary (NumElemsToInsert) where -- limit number of elements to insert to save time
    arbitrary = choose (1, 10000)

newtype FalsePositiveRate = FalsePositiveRate Double
    deriving (Num, Fractional, Random, Show)

instance Arbitrary FalsePositiveRate where
    arbitrary = choose (eps, 1-eps)
        where eps = 1e-6

instance Arbitrary L.ByteString where
    arbitrary = fmap L.pack arbitrary

instance Arbitrary S.ByteString where
    arbitrary = fmap S.pack arbitrary
