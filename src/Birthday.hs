{-# LANGUAGE BangPatterns #-}

module Birthday where

import           Data.Array    ((//))
import qualified Data.Array    as Arr
import           Data.List     (foldl', splitAt)
import qualified System.Random as Rand


yearLength :: Int
yearLength = 365

genRandBdays :: (Rand.RandomGen g) => g -> [Int]
genRandBdays = Rand.randomRs (1, yearLength)

canCelebrate :: [Int] -> Bool
canCelebrate bdays
    | length bdays < yearLength = False
    | any (\x -> x == (0 :: Int)) result = False
    | otherwise = True
    where
        bdayMask = Arr.array (1, yearLength) (zip [1..yearLength] (repeat 0))
        result = bdayMask // zip bdays (repeat 1)
        -- result = foldr (\num prev -> prev // [(num, 1)]) bdayMask bdays

likelihood :: (Rand.RandomGen g) => g -> Int -> Int -> Float
likelihood generator iterationCount sampleCount = average
    where
        stuff = genRandBdays generator
        sample = splitAt sampleCount
        (totalCount, successCount) = snd $ foldl' (\(s, (totalCount, successCount)) _ -> do
            let (nums, rest) = sample s
            let !r = canCelebrate nums
            (rest, (totalCount + 1, if r then successCount + 1 else successCount)))
            (stuff, (0, 0))
            [0..iterationCount - 1]
        average = fromIntegral successCount / fromIntegral totalCount
