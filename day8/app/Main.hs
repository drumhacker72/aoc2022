import Data.Array (Array, Ix, (!), bounds, indices, inRange, listArray)
import Data.Char (ord)

type Grid = Array (Int, Int) Int

lineToEdge :: Ix i => (i, i) -> (i -> i) -> i -> [i]
lineToEdge bs delta start
    | inRange bs start = start : lineToEdge bs delta (delta start)
    | otherwise = []

cardinalLines :: ((Int, Int), (Int, Int)) -> (Int, Int) -> [[(Int, Int)]]
cardinalLines bs start = map (\delta -> lineToEdge bs delta (delta start))
    [ \(r, c) -> (r+1, c)
    , \(r, c) -> (r-1, c)
    , \(r, c) -> (r, c+1)
    , \(r, c) -> (r, c-1)
    ]

heightLines :: Grid -> (Int, Int) -> [[Int]]
heightLines heights i = map (map (heights !)) $ cardinalLines (bounds heights) i

isVisible :: Grid -> (Int, Int) -> Bool
isVisible heights i = any id $ map (all (< (heights ! i))) $ heightLines heights i

viewDistance :: Int -> [Int] -> Int
viewDistance _ [] = 0
viewDistance initialHeight (h:hs)
    | initialHeight <= h = 1
    | otherwise = 1 + viewDistance initialHeight hs

scenicScore :: Grid -> (Int, Int) -> Int
scenicScore heights i = product $ map (viewDistance (heights ! i)) $ heightLines heights i

main :: IO ()
main = do
    trees <- map (map (\c -> ord c - ord '0')) . lines <$> readFile "day8.txt"
    let rows = length trees
        cols = length (head trees)
        heights = listArray ((1, 1), (rows, cols)) (concat trees)
    print $ length $ filter id $ map (isVisible heights) $ indices heights
    print $ maximum $ map (scenicScore heights) $ indices heights
