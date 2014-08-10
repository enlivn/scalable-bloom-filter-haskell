{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Hash.Hash
import qualified MutableBloomFilter as M
import System.Random (Random)
import Test.QuickCheck
import Control.Monad.ST (ST, runST)
--import Types

newtype FalsePositiveRate = FalsePositiveRate Double
    deriving (Num, Fractional, Random, Show)

instance Arbitrary FalsePositiveRate where
    arbitrary = choose (eps, 1-eps)
        where eps = 1e-6

runNTests :: Testable a => Int -> a -> IO ()
runNTests n = quickCheckWith (stdArgs{maxSuccess=n})

-- an element that was inserted is ALWAYS found
prop_elem_inserted_always_exists :: Hashable a => FalsePositiveRate -> a -> Bool
prop_elem_inserted_always_exists (FalsePositiveRate falsePositiveRate) element = runST $ M.fromList falsePositiveRate [element] >>=
                                                                          flip M.elem element

main :: IO ()
main = runNTests 500 (prop_elem_inserted_always_exists :: FalsePositiveRate -> Int -> Bool)
--main = do
--    let fp = 0.3080696608225276
--        mbps = runST $ mutBitsPerSlice `fmap` (M.new fp 1::ST s (MutableBloom s Int))
--    print (mbps)
--    res <- return $ (prop_elem_inserted_exists :: FalsePositiveRate -> String -> Bool) (FalsePositiveRate fp) "a"
--    print res
