import Data.Functor (void)
import Data.List (foldl', nub)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseMaybe, sepEndBy1)
import Text.Megaparsec.Char (char, eol)

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Direction = U | D | L | R
    deriving (Enum, Show)

direction :: Parser Direction
direction = (char 'U' >> pure U) <|> (char 'D' >> pure D) <|> (char 'L' >> pure L) <|> (char 'R' >> pure R)

motion :: Parser (Direction, Int)
motion = do
    d <- direction
    void $ char ' '
    s <- L.decimal
    pure (d, s)

isTouching :: (Int, Int) -> (Int, Int) -> Bool
isTouching (xh, yh) (xt, yt) = abs (xh - xt) <= 1 && abs (yh - yt) <= 1

doStep :: (Int, Int) -> Direction -> (Int, Int)
doStep (xh, yh) d = case d of
    U -> (xh, yh + 1)
    D -> (xh, yh - 1)
    L -> (xh - 1, yh)
    R -> (xh + 1, yh)

diagonals :: (Int, Int) -> [(Int, Int)]
diagonals (xt, yt) =
    [ (xt - 1, yt - 1)
    , (xt - 1, yt + 1)
    , (xt + 1, yt - 1)
    , (xt + 1, yt + 1)
    ]

follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow h@(xh, yh) t@(xt, yt)
    | isTouching h t = t
    | xh == xt && abs (yh - yt) == 2 = (xt, (yh + yt) `div` 2)
    | yh == yt && abs (xh - xt) == 2 = ((xh + xt) `div` 2, yt)
    | otherwise = case filter (isTouching h) (diagonals t) of
        [t'] -> t'
        _ -> error $ "shouldn't happen (ambiguous how to follow): " <> show h <> " " <> show t

tailPositions :: (Int, Int) -> (Int, Int) -> [Direction] -> [(Int, Int)]
tailPositions _ t [] = [t]
tailPositions h t (d:ds) = t : tailPositions h' t' ds
  where
    h' = doStep h d
    t' = follow h' t

tailPositions2 :: [(Int, Int)] -> [Direction] -> [(Int, Int)]
tailPositions2 [] _ = error "bad rope"
tailPositions2 (_:ts) [] = [last ts]
tailPositions2 (h:ts) (d:ds) = last ts : tailPositions2 (h':ts') ds
  where
    h' = doStep h d
    ts' = reverse $ snd $ foldl' (\(t0', acc) t1 -> let t1' = follow t0' t1 in (t1', t1' : acc)) (h', []) ts

main :: IO ()
main = do
    contents <- readFile "day9.txt"
    let motions = maybe (error "bad input") id $ parseMaybe (sepEndBy1 motion eol) contents
        motions' = concatMap (\(d, s) -> replicate s d) motions
    print $ length $ nub $ tailPositions (0, 0) (0, 0) motions'
    print $ length $ nub $ tailPositions2 (replicate 10 (0, 0)) motions'
