import Data.Functor (void)
import Data.List (elemIndex, foldl', nub, singleton)
import Data.Maybe (fromJust)
import Data.PQueue.Prio.Min (MinPQueue)
import Data.Set (Set)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), optional, parseMaybe, sepEndBy1, some, takeWhile1P, try)
import Text.Megaparsec.Char (char, eol)

import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as S

type Parser = Parsec Void String

pEdge :: Parser Int
pEdge = do
    tiles <- takeWhile1P (Just "'#' or '.'") (\c -> c == '#' || c == '.')
    pure $ fromJust $ elemIndex '.' tiles

data Direction = U | D | L | R
    deriving Show

pDirection :: Parser Direction
pDirection = (char '^' *> pure U) <|> (char 'v' *> pure D) <|> (char '<' *> pure L) <|> (char '>' *> pure R)

pTile :: Parser [Direction]
pTile = (char '.' *> pure []) <|> (singleton <$> pDirection)

pRow :: Parser (Int, [(Int, Direction)])
pRow = do
    void $ char '#'
    tiles <- some pTile
    void $ char '#'
    pure (length tiles, concat $ zipWith (\i ds -> map (i,) ds) [1..] tiles)

pInput :: Parser (Int, (Int, Int), [((Int, Int), Direction)], Int)
pInput = do
    entrance <- pEdge
    void eol
    rows <- sepEndBy1 (try pRow) eol
    let h = length rows
        w = case nub (map fst rows) of
            [x] -> x
            _ -> error "mismatched row lengths"
    let grid = concat $ zipWith (\y (_, row) -> map (\(x, t) -> ((x, y), t)) row) [1..] rows
    exit <- pEdge
    void $ optional eol
    pure (entrance, (w, h), grid, exit)

step :: (Int, Int) -> ((Int, Int), Direction) -> ((Int, Int), Direction)
step (maxX, maxY) ((x, y), d) = (p', d)
  where
    p' = case d of
        L -> if x == 1 then (maxX, y) else (x-1, y)
        R -> if x == maxX then (1, y) else (x+1, y)
        U -> if y == 1 then (x, maxY) else (x, y-1)
        D -> if y == maxY then (x, 1) else (x, y+1)

moves :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
moves (maxX, maxY) (x, y) = (x, y) : concat
    [ if x == 1 || (y == 0 || y == maxY+1) then [] else [(x-1, y)]
    , if x == maxX || (y == 0 || y == maxY+1) then [] else [(x+1, y)]
    , if y <= 1 then [] else [(x, y-1)]
    , if y >= maxY then [] else [(x, y+1)]
    ]

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

data Key = Key Int Int
    deriving (Eq, Show)
instance Ord Key where
    Key n1 d1 `compare` Key n2 d2 = (n1+d1) `compare` (n2+d2)

loop :: (Int -> (Int, Int) -> Bool) -> (Int, Int) -> (Int, Int) -> Set (Int, (Int, Int)) -> MinPQueue Key (Int, Int) -> Int
loop isFree exit bounds seen pq
    | p == exit = t+1
    | (t, p) `S.member` seen = loop isFree exit bounds seen pq'
    | otherwise = loop isFree exit bounds seen'
        $ foldl' (\acc p' -> PQ.insert (Key (t+1) $ 1 + manhattan p' exit) p' acc) pq' ps'
  where
    ((Key t _d, p), pq') = PQ.deleteFindMin pq
    ps' = filter (isFree (t+1)) $ moves bounds p
    seen' = S.insert (t, p) seen

main :: IO ()
main = do
    contents <- readFile "day24.txt"
    let (entrance, bounds@(_, maxY), blizzards, exit) = maybe (error "bad input") id $ parseMaybe pInput contents
        bsOverTime = iterate (map (step bounds)) blizzards
        occupiedOverTime :: [Set (Int, Int)]
        occupiedOverTime = map (S.fromList . map fst) bsOverTime
        isFree :: Int -> (Int, Int) -> Bool
        isFree t p = p `S.notMember` (occupiedOverTime !! t)
    let t1 = loop isFree (exit, maxY) bounds S.empty $ PQ.singleton (Key 0 999) (entrance, 0)
    print t1
    let t2 = loop isFree (entrance, 1) bounds S.empty $ PQ.singleton (Key t1 999) (exit, maxY+1)
        t3 = loop isFree (exit, maxY) bounds S.empty $ PQ.singleton (Key t2 999) (entrance, 0)
    print t3
