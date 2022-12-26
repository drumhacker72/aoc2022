import Data.Array (Array, Ix, (!), accumArray, array, assocs, bounds, inRange, ixmap, listArray)
import Data.Functor (void)
import Data.List (find, foldl1', transpose)
import Data.Maybe (fromJust, isJust)
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec (Parsec, (<|>), empty, many, optional, parseMaybe, takeWhileP, takeWhile1P, try)
import Text.Megaparsec.Char (char, eol)

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Turn = L | R
    deriving (Enum, Show)

data Tile = Open | Solid
    deriving (Enum, Show)

readTile :: Char -> Tile
readTile '#' = Solid
readTile '.' = Open
readTile _ = error "bad tile character"

pRow :: Parser (Int, [Tile])
pRow = do
    gap <- length <$> takeWhileP (Just "space") (== ' ')
    tiles <- takeWhile1P (Just "'#' or '.'") (\c -> c == '#' || c == '.')
    void eol
    pure (gap, map readTile tiles)

type Subgrid = Array (Int, Int) Tile

pRowBlock :: Int -> Parser Subgrid
pRowBlock y = do
    (gap, first) <- pRow
    let w = length first
    rest <- many $ try $ do
        (gap', row) <- pRow
        if gap == gap' && length row == w then pure row else empty
    let rows = first:rest
        h = length rows
    pure $ listArray ((gap+1, y+1), (gap+w, y+h)) (concat $ transpose rows)

pGrid :: Int -> Parser [Subgrid]
pGrid y = do
    (eol *> pure []) <|> do
        rb <- pRowBlock y
        let (_, (_, y')) = bounds rb
        rest <- pGrid y'
        pure (rb:rest)

pTurn :: Parser Turn
pTurn = (char 'L' *> pure L) <|> (char 'R' *> pure R)

data Path = Path Int [(Turn, Int)]
    deriving Show

pPath :: Parser Path
pPath = do
    first <- L.decimal
    segments <- many ((,) <$> pTurn <*> L.decimal)
    pure $ Path first segments
pInput :: Parser ([Subgrid], Path)
pInput = do
    grid <- pGrid 0
    path <- pPath
    void $ optional eol
    pure (grid, path)

type Grid = Array (Int, Int) (Maybe Tile)

assemble :: [Subgrid] -> Grid
assemble rowblocks = accumArray f Nothing ((minX, minY), (maxX, maxY)) $ concatMap assocs rowblocks
  where
    f Nothing t = Just t
    f (Just _) _ = error "unexpected"
    minX = minimum $ map (fst . fst . bounds) rowblocks
    minY = minimum $ map (snd . fst . bounds) rowblocks
    maxX = maximum $ map (fst . snd . bounds) rowblocks
    maxY = maximum $ map (snd . snd . bounds) rowblocks

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

dims :: Subgrid -> [Int]
dims g = [w, h]
  where
    ((minX, minY), (maxX, maxY)) = bounds g
    w = maxX - minX + 1
    h = maxY - minY + 1

hChunks :: Int -> Int -> Subgrid -> [Maybe Subgrid]
hChunks len x subgrid
    | x < minX = Nothing : hChunks len (x+len) subgrid
    | x > maxX = []
    | otherwise = Just (ixmap ((1, 1), (len, len)) (\(a,b) -> (x+a-1,minY+b-1)) subgrid) : hChunks len (x+len) subgrid
  where
    ((minX, minY), (maxX, _)) = bounds subgrid

squarify :: [Subgrid] -> (Int, Array (Int, Int) (Maybe Subgrid))
squarify subgrids = (len, listArray ((1, 1), (w, h)) $ concat $ transpose squares')
  where
    len = foldl1' gcd $ concatMap dims subgrids
    squares = map (hChunks len 1) subgrids
    h = length squares
    w = maximum $ map length squares
    squares' = map (\r -> r ++ replicate (w - length r) Nothing) squares

type V3 = (Int, Int, Int)

data Axis = PX | PY | PZ | NX | NY | NZ
    deriving (Eq, Ix, Ord, Show)

-- right hand (X grows right, Y up, Z out of the screen)

axisToV :: Axis -> V3
axisToV PX = (1, 0, 0)
axisToV PY = (0, 1, 0)
axisToV PZ = (0, 0, 1)
axisToV NX = (-1, 0, 0)
axisToV NY = (0, -1, 0)
axisToV NZ = (0, 0, -1)

vToAxis :: V3 -> Axis
vToAxis (1, 0, 0) = PX
vToAxis (0, 1, 0) = PY
vToAxis (0, 0, 1) = PZ
vToAxis (-1, 0, 0) = NX
vToAxis (0, -1, 0) = NY
vToAxis (0, 0, -1) = NZ
vToAxis _ = error "unaligned"

flipAxis :: Axis -> Axis
flipAxis PX = NX
flipAxis NX = PX
flipAxis PY = NY
flipAxis NY = PY
flipAxis PZ = NZ
flipAxis NZ = PZ

turnAround :: Axis -> V3 -> V3
turnAround PX (x, y, z) = (x, -z, y)
turnAround NX (x, y, z) = (x, z, -y)
turnAround PY (x, y, z) = (z, y, -x)
turnAround NY (x, y, z) = (-z, y, x)
turnAround PZ (x, y, z) = (-y, x, z)
turnAround NZ (x, y, z) = (y, -x, z)

turnAround' :: Axis -> Axis -> Axis
turnAround' = wrap . turnAround
  where
    wrap f = vToAxis . f . axisToV

crossProd :: V3 -> V3 -> V3
crossProd (x1, y1, z1) (x2, y2, z2) = (y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2)

crossProd' :: Axis -> Axis -> Axis
crossProd' a b = vToAxis $ crossProd (axisToV a) (axisToV b)

cubify :: Array (Int, Int) (Maybe a) -> [((Int, Int), (Axis, Axis))]
cubify squares = vecs (firstX, 1) Nothing (PX, NY)
  where
    firstX = fromJust $ find (\x -> isJust (squares ! (x, 1))) [1..]
    bs = bounds squares
    isGood p = inRange bs p && isJust (squares ! p)
    vecs :: (Int, Int) -> Maybe (Int, Int) -> (Axis, Axis) -> [((Int, Int), (Axis, Axis))]
    vecs p@(x, y) mprev (bx, by) = (p, (bx, by)) : concat
        [ let p' = (x-1, y) in if isGood p' && Just p' /= mprev then vecs p' (Just p) (localLeft bx, localLeft by) else []
        , let p' = (x+1, y) in if isGood p' && Just p' /= mprev then vecs p' (Just p) (localRight bx, localRight by) else []
        , let p' = (x, y-1) in if isGood p' && Just p' /= mprev then vecs p' (Just p) (localUp bx, localUp by) else []
        , let p' = (x, y+1) in if isGood p' && Just p' /= mprev then vecs p' (Just p) (localDown bx, localDown by) else []
        ]
      where
        localLeft = turnAround' by
        localRight = turnAround' (flipAxis by)
        localUp = turnAround' (flipAxis bx)
        localDown = turnAround' bx

data Cube = Cube Int (Array (Int, Int, Axis) (Tile, (Int, Int))) (Array Axis (Axis, Axis))

cubeLookup :: (Int, Int) -> (Axis, Axis) -> Cube -> (Tile, (Int, Int))
cubeLookup (x, y) (bx, by) (Cube len tiles bases)
    | bx == bx' && by == by' = tiles ! (x, y, face)
    | by == bx' && bx == flipAxis by' = tiles ! (y, len - x + 1, face)
    | bx == flipAxis bx' && by == flipAxis by' = tiles ! (len - x + 1, len - y + 1, face)
    | by == flipAxis bx' && bx == by' = tiles ! (len - y + 1, x, face)
    | otherwise = error "bad logic"
  where
    face = vToAxis $ crossProd (axisToV by) (axisToV bx)
    (bx', by') = bases ! face

adv2 :: (Int, Int) -> (Axis, Axis) -> Cube -> ((Int, Int), (Axis, Axis))
adv2 (x, y) (bx, by) (Cube len _ _)
    | x < len = ((x+1, y), (bx, by))
    | otherwise = ((1, y), (bx', by))
  where
    bx' = turnAround' (flipAxis by) bx

loop2 :: (Int, Int) -> (Axis, Axis) -> Cube -> Path -> (Int, Int)
loop2 (x, y) (bx, by) cube (Path 0 []) = snd (cubeLookup (x, y) (bx, by) cube)
loop2 (x, y) (bx, by) cube@(Cube len _ _) (Path 0 ((L, n):rest)) = loop2 (len - y + 1, x) (flipAxis by, bx) cube (Path n rest)
loop2 (x, y) (bx, by) cube@(Cube len _ _) (Path 0 ((R, n):rest)) = loop2 (y, len - x + 1) (by, flipAxis bx) cube (Path n rest)
loop2 p@(x, y) bases@(bx, by) cube (Path n rest) =
    let (p', bases') = adv2 (x, y) (bx, by) cube
     in case fst (cubeLookup p' bases' cube) of
        Open -> traceShow (snd $ cubeLookup p' bases' cube) $ loop2 p' bases' cube (Path (n-1) rest)
        Solid -> loop2 p bases cube (Path 0 rest)

main :: IO ()
main = do
    contents <- readFile "day22.txt"
    let (subgrids, path) = maybe (error "bad input") id $ parseMaybe pInput contents
        grid = assemble subgrids
    print $ uncurry password $ run grid (1, 1) Right_ path

    let (len, squares) = squarify subgrids
        cubified = cubify squares
        tiles = array ((1, 1, PX), (len, len, NZ))
                [ ((x, y, crossProd' by bx), (fromJust (squares ! (sx, sy)) ! (x, y), ((sx-1)*len+x, (sy-1)*len+y)))
                | ((sx, sy), (bx, by)) <- cubified, x <- [1..len], y <- [1..len]
                ]
        bases = array (PX, NZ) [ (crossProd' by bx, (bx, by)) | (_, (bx, by)) <- cubified ]
        cube = Cube len tiles bases
    print $ loop2 (1, 1) (PX, NY) cube path
