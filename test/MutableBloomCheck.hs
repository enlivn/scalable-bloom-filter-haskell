{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Test.QuickCheck
import Hash.Hash
import qualified MutableBloomFilter as M
import System.Random (Random)
import Control.Monad.ST (ST, runST)
import Types

newtype FalsePositiveRate = FalsePositiveRate Double
    deriving (Num, Fractional, Random, Show)

instance Arbitrary FalsePositiveRate where
    arbitrary = choose (eps, 1-eps)
        where eps = 0.2 -- 1e-6

runNTests :: Testable a => Int -> a -> IO ()
runNTests n = verboseCheckWith (stdArgs{maxSuccess=n})

prop_one_exists :: Hashable a => FalsePositiveRate -> a -> Bool
prop_one_exists (FalsePositiveRate falsePositiveRate) element = runST $ M.fromList falsePositiveRate [element] >>=
                                                                        (flip M.elem) element

main :: IO ()
main = runNTests 500 (prop_one_exists :: FalsePositiveRate -> Int -> Bool)
--main = do
    --let mbps = runST $ mutBitsPerSlice `fmap` (M.new 0.24 1::ST s (MutableBloom s Int))
    --print (mbps)
    --res <- return $ (prop_one_exists :: FalsePositiveRate -> String -> Bool) 0.24 "a"
    --print res
