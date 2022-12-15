import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, getBounds, newArray, readArray, writeArray)
import Data.Functor (void)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepBy1, sepEndBy1)
import Text.Megaparsec.Char (char, eol, string)

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

mkGrid :: ((Int, Int), (Int, Int)) -> ST s (STUArray s (Int, Int) Bool)
mkGrid = (`newArray` False)

pour :: STUArray s (Int, Int) Bool -> (Int, Int) -> ST s Bool
pour grid (x, y) = do
    (_, (_, bottom)) <- getBounds grid
    if y >= bottom
        then return False
        else do
            down <- readArray grid (x, y+1)
            downLeft <- readArray grid (x-1, y+1)
            downRight <- readArray grid (x+1, y+1)
            case (down, downLeft, downRight) of
                (False, _, _) -> pour grid (x, y+1)
                (True, False, _) -> pour grid (x-1, y+1)
                (True, True, False) -> pour grid (x+1, y+1)
                (True, True, True) -> do
                    writeArray grid (x, y) True
                    return True                

numPours :: STUArray s (Int, Int) Bool -> Int -> ST s Int
numPours grid n = do
    settled <- pour grid (500, 0)
    if settled
        then numPours grid (n+1)
        else return n

main :: IO ()
main = do
    contents <- readFile "day14.txt"
    let paths = maybe (error "bad input") id $ parseMaybe pInput contents
        points = concatMap unrollPath paths
        left = (minimum $ 500 : map fst points) - 2
        right = (maximum $ 500 : map fst points) + 2
        top = minimum $ 0 : map snd points
        bottom = maximum $ 0 : map snd points
    print $ runST $ do
        grid <- mkGrid ((left, top), (right, bottom))
        forM_ points $ \point -> writeArray grid point True
        numPours grid 0
