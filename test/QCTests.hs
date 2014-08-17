{-|
Module      : QCTests
Description : QuickCheck tests integration with cabal
-}

module Main where

import qualified MutableBloomFilterCheck as M
import qualified ScalableBloomFilterCheck as SB
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test, defaultMain)
import TestTypes

tests :: IO [Test]
tests = return [
            testProperty "element inserted always exists"
                         (M.prop_elem_inserted_always_exists :: FalsePositiveRate -> Char -> Bool),
            testProperty "element inserted always exists"
                         (M.prop_elem_inserted_always_exists :: FalsePositiveRate -> Int -> Bool),
            testProperty "element inserted always exists"
                         (M.prop_elem_inserted_always_exists :: FalsePositiveRate -> Float -> Bool),
            testProperty "element inserted always exists"
                         (M.prop_elem_inserted_always_exists :: FalsePositiveRate -> Double -> Bool),
            testProperty "element inserted always exists"
                         (M.prop_elem_inserted_always_exists :: FalsePositiveRate -> String -> Bool),
            testProperty "element inserted always exists"
                         (M.prop_elem_inserted_always_exists :: FalsePositiveRate -> L.ByteString -> Bool),
            testProperty "element inserted always exists"
                         (M.prop_elem_inserted_always_exists :: FalsePositiveRate -> S.ByteString -> Bool),
            testProperty "filter count <= num elements inserted"
                         (M.prop_filt_count_is_less_than_or_equal_num_elems_inserted :: LimitNumElems -> Bool),
            testProperty "element inserted always exists"
                         (SB.prop_elem_inserted_always_exists :: FalsePositiveRate -> Char -> Bool),
            testProperty "element inserted always exists"
                         (SB.prop_elem_inserted_always_exists :: FalsePositiveRate -> Int -> Bool),
            testProperty "element inserted always exists"
                         (SB.prop_elem_inserted_always_exists :: FalsePositiveRate -> Float -> Bool),
            testProperty "element inserted always exists"
                         (SB.prop_elem_inserted_always_exists :: FalsePositiveRate -> Double -> Bool),
            testProperty "element inserted always exists"
                         (SB.prop_elem_inserted_always_exists :: FalsePositiveRate -> String -> Bool),
            testProperty "element inserted always exists"
                         (SB.prop_elem_inserted_always_exists :: FalsePositiveRate -> L.ByteString -> Bool),
            testProperty "element inserted always exists"
                         (SB.prop_elem_inserted_always_exists :: FalsePositiveRate -> S.ByteString -> Bool),
            testProperty "filter scales properly"
                         (SB.prop_filter_scales :: NumElemsToForceScaling -> Bool),
            testProperty "filter does not scale if not enough elements inserted"
                         (SB.prop_filter_does_not_scale :: NumElemsForNoScaling -> Bool)
        ]

main :: IO ()
main = defaultMain =<< tests -- M.main >> S.main
