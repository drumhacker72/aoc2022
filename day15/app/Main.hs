import Data.Functor (void)
import Data.List (foldl')
import Data.PQueue.Prio.Min (MinPQueue)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepEndBy1)
import Text.Megaparsec.Char (eol, string)

import qualified Data.IntMap.Strict as IM
import qualified Data.PQueue.Prio.Min as PQ
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pInteger :: Parser Int
pInteger = L.signed (pure ()) L.decimal

pPoint :: Parser (Int, Int)
pPoint = do
    void $ string "x="
    x <- pInteger
    void $ string ", y="
    y <- pInteger
    pure (x, y)

pReport :: Parser ((Int, Int), (Int, Int))
pReport = do
    void $ string "Sensor at "
    sensor <- pPoint
    void $ string ": closest beacon is at "
    beacon <- pPoint
    pure (sensor, beacon)

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

coverageOnRow :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
coverageOnRow sensor@(sx, sy) beacon row = [(col, row) | col <- cols]
  where
    radius = manhattan sensor beacon
    span_ = radius - abs (row - sy)
    cols = [sx-span_..sx+span_]

data Sensor = Sensor (Int, Int) Int
type Rect = ((Int, Int), (Int, Int))

inRect :: Rect -> (Int, Int) -> Bool
inRect ((x1, y1), (x2, y2)) (a, b) = x1 <= a && a <= x2 && y1 <= b && b <= y2

rectCorners :: Rect -> [(Int, Int)]
rectCorners ((x1, y1), (x2, y2)) =
    [ (x1, y1)
    , (x1, y2)
    , (x2, y1)
    , (x2, y2)
    ]

sensorSeesRect :: Rect -> Sensor -> Bool
sensorSeesRect rect@((rx1, ry1), (rx2, ry2)) (Sensor sensor@(sx, sy) radius) = sensorInRect || sensorSeesCorner || sensorSeesEdge
  where
    sensorInRect = inRect rect sensor
    sensorSeesCorner = any id $ map (\c -> manhattan sensor c <= radius) (rectCorners rect)
    sensorSeesEdge = any id $ map (\p -> inRect rect p && manhattan sensor p <= radius)
                            $ [(sx, ry1), (sx, ry2), (rx1, sy), (rx2, sy)]

rectIsPoint :: Rect -> Bool
rectIsPoint ((x1, y1), (x2, y2)) = x1 == x2 && y1 == y2

split :: Rect -> [Rect]
split rect@((x1, y1), (x2, y2))
    | rectIsPoint rect = error "no further splits"
    | x1 == x2 = [((x1, y1), (x1, ys)), ((x1, ys+1), (x1, y2))]
    | y1 == y2 = [((x1, y1), (xs, y1)), ((xs+1, y1), (x2, y1))]
    | otherwise =
        [ ((x1, y1), (xs, ys))
        , ((xs+1, y1), (x2, ys))
        , ((x1, ys+1), (xs, y2))
        , ((xs+1, ys+1), (x2, y2))
        ]
  where
    xs = (x1 + x2) `div` 2
    ys = (y1 + y2) `div` 2

rectFullyInSensor :: Rect -> Sensor -> Bool
rectFullyInSensor rect (Sensor sensor radius) = all id $ map (\c -> manhattan sensor c <= radius) (rectCorners rect)

step :: [Sensor] -> Rect -> [(Int, (Rect, [Sensor]))]
step sensors rect = map (\r' ->
    let sensors' = filter (sensorSeesRect r') sensors
     in (length sensors', (r', sensors'))) (split rect)

loop :: MinPQueue Int (Rect, [Sensor]) -> Rect
loop pq = case PQ.deleteFindMin pq of
    ((0, (r, _)), _) -> r
    ((_n, (r, sensors)), pq')
        | rectIsPoint r -> loop pq'
        | any (rectFullyInSensor r) sensors -> loop pq'
        | otherwise -> loop $ foldl' (\pq'' (n, rs) -> PQ.insert n rs pq'') pq' $ step sensors r

main :: IO ()
main = do
    contents <- readFile "day15.txt"
    let reports = maybe (error "bad input") id $ parseMaybe (sepEndBy1 pReport eol) contents
        y = 2_000_000
        coverage = foldl' (\acc (sensor, beacon@(bx, by)) ->
            let acc' = if by == y then IM.insert bx True acc else acc
             in foldl' (\acc'' (x, _) -> IM.union acc'' (IM.singleton x False)) acc' (coverageOnRow sensor beacon y)
            ) IM.empty reports
    print $ length $ filter not $ IM.elems coverage

    let sensors = map (\(s, b) -> Sensor s (manhattan s b)) reports
    let (p@(px, py), p') = loop $ PQ.singleton (length sensors) (((0, 0), (4_000_000, 4_000_000)), sensors)
    if p == p' then print (px * 4_000_000 + py) else error $ "expected one point, got: " <> show (p, p')
