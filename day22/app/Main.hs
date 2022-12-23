import Data.Array (Array, (!), array, bounds, inRange)
import Data.Functor (void)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), many, optional, parseMaybe, sepEndBy1, some)
import Text.Megaparsec.Char (char, eol)

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Turn = L | R
    deriving (Enum, Show)

data Tile = Open | Solid
    deriving (Enum, Show)

pTile :: Parser (Maybe Tile)
pTile = (char ' ' *> pure Nothing) <|> (char '.' *> pure (Just Open)) <|> (char '#' *> pure (Just Solid))

pRow :: Parser (Int, [(Int, Maybe Tile)])
pRow = do
    ts <- some pTile
    let w = length ts
        ts' = ts ++ repeat Nothing
        idxTs = zipWith (,) [1..] ts'
    pure (w, idxTs)

type Grid = Array (Int, Int) (Maybe Tile)

pGrid :: Parser Grid
pGrid = do
    sizedRows <- sepEndBy1 pRow eol
    let w = maximum $ map fst sizedRows
        rows = map snd sizedRows
        g = zipWith (\y xts -> map (\(x, t) -> ((x, y), t)) (take w xts)) [1..] rows
    pure $ array ((1, 1), (w, length g)) (concat g)

pTurn :: Parser Turn
pTurn = (char 'L' *> pure L) <|> (char 'R' *> pure R)

data Path = Path Int [(Turn, Int)]
    deriving Show

pPath :: Parser Path
pPath = do
    first <- L.decimal
    segments <- many ((,) <$> pTurn <*> L.decimal)
    pure $ Path first segments

pInput :: Parser (Grid, Path)
pInput = do
    grid <- pGrid
    void eol
    path <- pPath
    void $ optional eol
    pure (grid, path)

{-
gridLines :: Grid -> [String]
gridLines grid =
    let (_, (w, h)) = bounds grid
     in map (\y -> map (\x -> case grid ! (x, y) of
        Nothing -> ' '
        Just Open -> '.'
        Just Solid -> '#') [1..w]) [1..h]
-}

type Advancer = ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int)

adv :: Grid -> Int -> Advancer -> (Int, Int) -> (Int, Int) -> (Int, Int)
adv _ 0 _ _ p = p
adv grid n f lastGood current = case grid ! next of
    Just Open -> adv grid (n-1) f next next
    Just Solid -> lastGood
    Nothing -> adv grid n f lastGood next
  where
    next = f (bounds grid) current

left, right, up, down :: Advancer
left bs@(_, (maxX, _)) (x, y) = let p' = (x-1, y) in if inRange bs p' then p' else (maxX, y)
right bs (x, y) = let p' = (x+1, y) in if inRange bs p' then p' else (1, y)
up bs@(_, (_, maxY)) (x, y) = let p' = (x, y-1) in if inRange bs p' then p' else (x, maxY)
down bs (x, y) = let p' = (x, y+1) in if inRange bs p' then p' else (x, 1)

data Facing = Right_ | Down | Left_ | Up
    deriving (Enum, Show)

turn :: Turn -> Facing -> Facing
turn L Right_ = Up
turn L Down = Right_
turn L Left_ = Down
turn L Up = Left_
turn R Right_ = Down
turn R Down = Left_
turn R Left_ = Up
turn R Up = Right_

delta :: Facing -> Advancer
delta Right_ = right
delta Down = down
delta Left_ = left
delta Up = up

run :: Grid -> (Int, Int) -> Facing -> Path -> ((Int, Int), Facing)
run grid p dir (Path n rest) =
    let p' = adv grid n (delta dir) p p
     in case rest of
        [] -> (p', dir)
        ((t, n'):rest') -> run grid p' (turn t dir) (Path n' rest')

password :: (Int, Int) -> Facing -> Int
password (x, y) dir = y * 1000 + x * 4 + fromEnum dir

main :: IO ()
main = do
    contents <- readFile "day22.txt"
    let (grid, path) = maybe (error "bad input") id $ parseMaybe pInput contents
    print $ uncurry password $ run grid (1, 1) Right_ path
