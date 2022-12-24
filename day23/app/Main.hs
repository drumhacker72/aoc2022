import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseMaybe, sepEndBy1, some)
import Text.Megaparsec.Char (char, eol)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Parser = Parsec Void String

pPosition :: Parser Bool
pPosition = (char '.' *> pure False) <|> (char '#' *> pure True)

pRow :: Parser (Set Int)
pRow = do
    elves <- some pPosition
    pure $ S.fromList $ map fst $ filter snd $ zip [1..] elves

pGrid :: Parser (Set (Int, Int))
pGrid = do
    rows <- sepEndBy1 pRow eol
    pure $ S.unions $ map (\(y, row) -> S.mapMonotonic (\x -> (x, y)) row) $ zip [1..] rows

adjacents, northAdjacents, southAdjacents, westAdjacents, eastAdjacents :: (Int, Int) -> [(Int, Int)]
adjacents (x, y) =
    [ (x-1, y-1), (x, y-1), (x+1, y-1)
    , (x-1, y)            , (x+1, y)
    , (x-1, y+1), (x, y+1), (x+1, y+1)
    ]
northAdjacents (x, y) = [(x-1, y-1), (x, y-1), (x+1, y-1)]
southAdjacents (x, y) = [(x-1, y+1), (x, y+1), (x+1, y+1)]
westAdjacents (x, y) = [(x-1, y-1), (x-1, y), (x-1, y+1)]
eastAdjacents (x, y) = [(x+1, y-1), (x+1, y), (x+1, y+1)]

type Checks = [((Int, Int) -> [(Int, Int)], (Int, Int) -> (Int, Int))]
initialChecks :: Checks
initialChecks = cycle
    [ (northAdjacents, \(x, y) -> (x, y-1))
    , (southAdjacents, \(x, y) -> (x, y+1))
    , (westAdjacents, \(x, y) -> (x-1, y))
    , (eastAdjacents, \(x, y) -> (x+1, y))
    ]

proposal :: Checks -> Set (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
proposal checks elves p
    | all (`S.notMember` elves) (adjacents p) = Nothing
    | all (`S.notMember` elves) (fst (checks !! 0) p) = Just $ snd (checks !! 0) p
    | all (`S.notMember` elves) (fst (checks !! 1) p) = Just $ snd (checks !! 1) p
    | all (`S.notMember` elves) (fst (checks !! 2) p) = Just $ snd (checks !! 2) p
    | all (`S.notMember` elves) (fst (checks !! 3) p) = Just $ snd (checks !! 3) p
    | otherwise = Nothing

addProposal :: Set (Int, Int) -> Checks -> Map (Int, Int) [(Int, Int)] -> (Int, Int) -> Map (Int, Int) [(Int, Int)]
addProposal elves checks acc p =
    case proposal checks elves p of
        Nothing -> M.insert p [p] acc
        Just p' -> M.insert p' (p : M.findWithDefault [] p' acc) acc

resolve :: Map (Int, Int) [(Int, Int)] -> Set (Int, Int)
resolve = S.fromList . concat . M.mapWithKey resolveOne

resolveOne :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
resolveOne p' [_] = [p']
resolveOne _ origs = origs

loop :: Int -> Checks -> Set (Int, Int) -> Set (Int, Int)
loop 0 _ elves = elves
loop n checks elves
    | any (isJust . proposal checks elves) elves =
        loop (n-1) (tail checks) $ resolve $ foldl' (addProposal elves checks) M.empty $ S.elems elves
    | otherwise = elves

loop2 :: Int -> Checks -> Set (Int, Int) -> Int
loop2 n checks elves
    | any (isJust . proposal checks elves) elves =
        loop2 (n+1) (tail checks) $ resolve $ foldl' (addProposal elves checks) M.empty $ S.elems elves
    | otherwise = n

main :: IO ()
main = do
    contents <- readFile "day23.txt"
    let elves = maybe (error "bad input") id $ parseMaybe pGrid contents
        final = loop 10 initialChecks elves
        minX = minimum $ map fst $ S.elems final
        maxX = maximum $ map fst $ S.elems final
        minY = minimum $ map snd $ S.elems final
        maxY = maximum $ map snd $ S.elems final
    print $ (maxY - minY + 1) * (maxX - minX + 1) - length elves
    print $ loop2 1 initialChecks elves
