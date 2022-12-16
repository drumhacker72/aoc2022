import Data.Functor (void)
import Data.List (foldl')
import Data.Set (Set)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepBy1, sepEndBy1)
import Text.Megaparsec.Char (char, eol, string)

import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pPoint :: Parser (Int, Int)
pPoint = do
    x <- L.decimal
    void $ char ','
    y <- L.decimal
    pure (x, y)

pPath :: Parser [(Int, Int)]
pPath = sepBy1 pPoint (string " -> ")

pInput :: Parser [[(Int, Int)]]
pInput = sepEndBy1 pPath eol

unrollSegment :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
unrollSegment (x1, y1) (x2, y2) = (x1, y1) : case (compare x1 x2, compare y1 y2) of
    (EQ, EQ) -> []
    (EQ, LT) -> unrollSegment (x1, y1 + 1) (x2, y2)
    (EQ, GT) -> unrollSegment (x1, y1 - 1) (x2, y2)
    (LT, EQ) -> unrollSegment (x1 + 1, y1) (x2, y2)
    (GT, EQ) -> unrollSegment (x1 - 1, y1) (x2, y2)
    _ -> error "unexpected: unaligned line"

unrollPath :: [(Int, Int)] -> [(Int, Int)]
unrollPath [] = error "unexpected: empty path"
unrollPath [p] = [p]
unrollPath (p1:p2:rest) = unrollSegment p1 p2 ++ unrollPath (p2:rest)

data Grid = Grid
    { _blocks :: Set (Int, Int)
    , _bottom :: Int
    }

pour :: Grid -> (Int, Int) -> Maybe Grid
pour grid@(Grid blocks bottom) (x, y) =
    if y >= bottom
    then Nothing
    else
        let down = S.member (x, y+1) blocks
            downLeft = S.member (x-1, y+1) blocks
            downRight = S.member (x+1, y+1) blocks
         in case (down, downLeft, downRight) of
            (False, _, _) -> pour grid (x, y+1)
            (True, False, _) -> pour grid (x-1, y+1)
            (True, True, False) -> pour grid (x+1, y+1)
            (True, True, True) -> Just $ Grid (S.insert (x, y) blocks) bottom

numPours :: Grid -> Int -> Int
numPours grid n =
    case pour grid (500, 0) of
        Nothing -> n
        Just grid' -> numPours grid' (n+1)

main :: IO ()
main = do
    contents <- readFile "day14.txt"
    let paths = maybe (error "bad input") id $ parseMaybe pInput contents
        points = concatMap unrollPath paths
        bottom = maximum $ 0 : map snd points
        blocks = foldl' (flip S.insert) S.empty points
    print $ numPours (Grid blocks bottom) 0
