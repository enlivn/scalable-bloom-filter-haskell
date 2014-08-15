{-|
Module      : Main
Description : QuickCheck tests for Mutable bloom filter
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Hash.Hash
import qualified MutableBloomFilter as M
import System.Random (Random)
import Test.QuickCheck
import Control.Monad.ST (ST, runST)
import Data.Word (Word32)
import Data.ByteString.Lazy as L (ByteString, pack)
import Data.ByteString as S (ByteString, pack)
--import Types

newtype FalsePositiveRate = FalsePositiveRate Double
    deriving (Num, Fractional, Random, Show)

instance Arbitrary FalsePositiveRate where
    arbitrary = choose (eps, 1-eps)
        where eps = 1e-6

instance Arbitrary L.ByteString where
    arbitrary = fmap L.pack arbitrary

instance Arbitrary S.ByteString where
    arbitrary = fmap S.pack arbitrary

runNTests :: Testable a => Int -> a -> IO ()
runNTests n = quickCheckWith (stdArgs{maxSuccess=n})

-- an element that was inserted is ALWAYS found
prop_elem_inserted_always_exists :: Hashable a => FalsePositiveRate -> a -> Bool
prop_elem_inserted_always_exists (FalsePositiveRate falsePositiveRate) element = runST $ M.fromList falsePositiveRate [element] >>=
                                                                                 flip M.elem element

main :: IO ()
main = do
    runNTests 5000 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Char -> Bool)
    runNTests 5000 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Int -> Bool)
    runNTests 5000 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Float -> Bool)
    runNTests 5000 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Double -> Bool)
    runNTests 5000 (prop_elem_inserted_always_exists :: FalsePositiveRate -> String -> Bool)
    runNTests 5000 (prop_elem_inserted_always_exists :: FalsePositiveRate -> L.ByteString -> Bool)
    runNTests 5000 (prop_elem_inserted_always_exists :: FalsePositiveRate -> S.ByteString -> Bool)

--main = do
--    let fp = 0.3080696608225276
--        n  = 2100937750 :: Word32
--        --res :: Bool
--        --res = runST $ (M.new fp n::ST s (M.MutableBloom s Int)) >>= (flip M.elem) 20
--        mbps = runST $ mutBitsPerSlice `fmap` (M.new fp n::ST s (M.MutableBloom s Int))
 --   --print res
 --   print mbps
--    res <- return $ (prop_elem_inserted_exists :: FalsePositiveRate -> String -> Bool) (FalsePositiveRate fp) "a"
--    print res
