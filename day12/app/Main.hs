import Data.Array (Array, (!), assocs, bounds, inRange, listArray)
import Data.List (find, foldl')
import Data.Maybe (catMaybes)
import Data.PriorityQueue.FingerTree (PQueue)
import Data.Set (Set)

import qualified Data.PriorityQueue.FingerTree as PQ
import qualified Data.Set as S

adjacents :: (Int, Int) -> [(Int, Int)]
adjacents (r, c) =
    [ (r - 1, c)
    , (r + 1, c)
    , (r, c - 1)
    , (r, c + 1)
    ]

isValidMove :: Array (Int, Int) Char -> Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isValidMove grid seen from to
    | not (inRange (bounds grid) to) = False
    | S.member to seen = False
    | otherwise = src >= dst || succ src == dst
  where
    src = let s = grid ! from in if s == 'S' then 'a' else s
    dst = let d = grid ! to in if d == 'E' then 'z' else d

shortest :: Array (Int, Int) Char -> Set (Int, Int) -> PQueue Int (Int, Int) -> Maybe Int
shortest grid seen pq = case PQ.minViewWithKey pq of
    Nothing -> Nothing
    Just ((len, pos), pq')
        | S.member pos seen -> shortest grid seen pq'
        | otherwise ->
            let seen' = S.insert pos seen
            in if grid ! pos == 'E' then Just len else shortest grid seen'
                $ foldl' (\pq'' pos' -> if isValidMove grid seen' pos pos' then PQ.insert (len+1) pos' pq'' else pq'') pq'
                $ adjacents pos

main :: IO ()
main = do
    contents <- lines <$> readFile "day12.txt"
    let rows = length contents
        cols = length (head contents)
        grid = listArray ((1, 1), (rows, cols)) (concat contents)
        start = case find (\(_, e) -> e == 'S') (assocs grid) of
            Just (pos, _) -> pos
            Nothing -> error "no start found"
    let steps = case shortest grid S.empty (PQ.singleton 0 start) of
            Nothing -> error "failed to find path"
            Just n -> n
    print steps

    -- TODO: produce full list of distances from 'E' and pick lowest that starts from 'a'
    -- for better performance
    let starts = map fst $ filter (\(_, e) -> e == 'a') (assocs grid)
    print $ minimum $ catMaybes $ map (\s -> shortest grid S.empty $ PQ.singleton 0 s) starts
