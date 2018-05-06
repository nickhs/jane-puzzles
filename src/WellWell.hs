{-# LANGUAGE ScopedTypeVariables #-}

module WellWell where

-- https://www.janestreet.com/puzzles/well-well-well/

import Data.Array (Array, (//), (!))
import qualified Data.Array as Arr
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intersperse, intercalate, foldl')
import Text.Printf

import Debug.Trace

type Coord = (Int, Int)
data Tile = Tile Int Float deriving (Show, Eq)

-- zero index from bottom left
type Grid = Array Coord Tile

rowLen :: Int
rowLen = 7

colLen :: Int
colLen = 7

bounds :: (Coord, Coord)
bounds = ((0, 0), (rowLen - 1, colLen - 1))

crossProduct :: [a] -> [b] -> [(b, a)]
crossProduct as bs = [(b, a) | a <- as, b <- bs]

inp :: [Int]
inp = [
  01, 05, 27, 22, 28, 40, 14,
  39, 13, 17, 30, 41, 12, 02,
  32, 35, 24, 25, 19, 47, 34,
  16, 33, 10, 42, 07, 44, 18,
  03, 08, 45, 37, 04, 21, 20,
  15, 46, 38, 06, 26, 48, 49,
  09, 23, 31, 29, 11, 36, 43]

allCoords :: [Coord]
allCoords = crossProduct [0..colLen - 1] [0..rowLen - 1]

initialGrid :: Grid
initialGrid = Arr.array bounds tiles
    where
        tiles = zip allCoords (map (\x -> Tile x 0) inp)

orthAdjNeighbours :: ([Coord] -> [Coord]) -> Coord -> [Coord]
orthAdjNeighbours clipper (x, y) = clipper [left, right, top, bottom]
    where
        left = (x - 1, y)
        right = (x + 1, y)
        top = (x, y - 1)
        bottom = (x, y + 1)

clip :: (Coord, Coord) -> [Coord] -> [Coord]
clip ((minX, minY), (maxX, maxY)) = filter isInBounds
    where
        isInBounds (x, y) = isInX x && isInY y
        isInX x = minX <= x && x <= maxX
        isInY y = minY <= y && y <= maxY

wellNeighbours :: Coord -> [Coord]
wellNeighbours = orthAdjNeighbours (clip bounds)

{--
   Function that searches for any water that should be flowing
   and then moves it accordingly
--}
flow' :: Grid -> Grid
flow' grid = foldl' tileFlow grid allCoords

{-- Moves water from tile to neighbours.
    Determines if there's a gradient of water to move, and moves it
--}
tileFlow :: Grid -> Coord -> Grid
tileFlow grid coord = do
    let (Tile tMaxDepth tCurDepth) = grid ! coord
    let neighbours = wellNeighbours coord
    let neighbours' = map (\c -> (c, grid ! c)) neighbours
    let flowers = filter (\(c, Tile maxDepth curDepth) -> (fromIntegral maxDepth - curDepth) > (fromIntegral tMaxDepth - tCurDepth)) neighbours'

    if null flowers
    then grid
    else do
        let minDelta = minimum $ map (\(_, Tile maxDepth curDepth) -> (fromIntegral maxDepth - curDepth) - (fromIntegral tMaxDepth - tCurDepth)) flowers
        let waterSize = min minDelta tCurDepth
        let flowRate = waterSize / fromIntegral (length flowers)
        let newCells = map (\(c, Tile maxDepth curDepth) -> if curDepth + flowRate > fromIntegral maxDepth then error "wat ??" else (c, Tile maxDepth (curDepth + flowRate))) flowers
        let newGrid = grid // (((coord, Tile tMaxDepth (tCurDepth - waterSize))) : newCells)
        newGrid
        -- trace (showGrid newGrid) newGrid

flowUntilStable :: Grid -> Int -> Grid
flowUntilStable g count
  | count > 1000 = g
  | newG == g = g
  | otherwise = flowUntilStable newG (count + 1)
  where
      newG = flow' g
      newG' = trace (diffCharWise (showGrid newG) (showGrid g)) newG

drip :: Grid -> Grid
drip grid = flowUntilStable (grid // [((0, 0), Tile 1 1)]) 0

showGrid :: Grid -> String
showGrid g = do
    let cellWidth = 6 :: Int -- for curDepth maxDepth
    let cellHeight = 1

    let lineBreak = concatMap (\_ -> " " ++ concat (replicate (cellWidth - 1) "-")) [0..colLen - 1]
    let mkRow y = concatMap (\x -> do
            let (Tile maxDepth curDepth) = g ! (x, y)
            printf "|%5.2f" (fromIntegral maxDepth - curDepth)) [0..colLen - 1]
    let rows = concat $ intersperse ("\n" ++ lineBreak ++ "\n") $ map mkRow [0..rowLen - 1] :: String
    concat [lineBreak, "\n", rows, "\n"]

diffCharWise :: String -> String -> String
diffCharWise a b = go 0 False
    where
        defaultColorEscapeCode = "\x1b[0m"
        highlightColorCode = "\x1b[2m"
        isCharDiff idx = a !! idx == b !! idx
        go :: Int -> Bool -> String
        go idx isDiff = if idx >= length a
                        then ""
                        else
                            case (isDiff, isCharDiff idx) of
                                (True, False) -> defaultColorEscapeCode ++ [b !! idx] ++ go (idx + 1) False
                                (False, True) -> highlightColorCode ++ [b !! idx] ++ go (idx + 1) True
                                (False, False) -> [b !! idx] ++ go (idx + 1) isDiff
                                (True, True) -> [b !! idx] ++ go (idx + 1) isDiff
