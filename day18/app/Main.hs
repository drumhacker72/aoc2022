import Data.Functor (void)
import Data.List (foldl')
import Data.PQueue.Prio.Min (MinPQueue)
import Data.Set (Set)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepEndBy1)
import Text.Megaparsec.Char (char, eol)

import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Point = (Int, Int, Int)

pPosition :: Parser Point
pPosition = do
    x <- L.decimal
    void $ char ','
    y <- L.decimal
    void $ char ','
    z <- L.decimal
    pure (x, y, z)

adjacents :: Point -> [Point]
adjacents (x, y, z) =
    [ (x-1, y, z), (x+1, y, z)
    , (x, y-1, z), (x, y+1, z)
    , (x, y, z-1), (x, y, z+1)
    ]

px, py, pz :: Point -> Int
px (x, _, _) = x
py (_, y, _) = y
pz (_, _, z) = z

inBounds :: (Point, Point) -> Point -> Bool
inBounds ((minX, minY, minZ), (maxX, maxY, maxZ)) (x, y, z) =
    minX <= x && x <= maxX && minY <= y && y <= maxY && minZ <= z && z <= maxZ

hasPath :: (Point, Point) -> Set Point -> Point -> Bool
hasPath bounds cubes end = not $ null $ findPath bounds cubes end

findPath :: (Point, Point) -> Set Point -> Point -> [Point]
findPath bounds cubes end = loop (PQ.singleton 0 (fst bounds, [])) S.empty
  where
    loop :: MinPQueue Int (Point, [Point]) -> Set Point -> [Point]
    loop pq seen = case PQ.minViewWithKey pq of
        Nothing -> []
        Just ((n, (p, path)), pq')
            | p == end -> reverse (p : path)
            | not (inBounds bounds p) || p `S.member` seen || p `S.member` cubes ->
                loop pq' seen
            | otherwise ->
                let pq'' = foldl' (\acc adj -> PQ.insert (n+1) (adj, p:path) acc) pq' (adjacents p)
                    seen' = S.insert p seen
                in loop pq'' seen'

main :: IO ()
main = do
    contents <- readFile "day18.txt"
    let cubes = S.fromList $ maybe (error "bad input") id $ parseMaybe (sepEndBy1 pPosition eol) contents
        minX = minimum $ map px $ S.elems cubes
        maxX = maximum $ map px $ S.elems cubes
        minY = minimum $ map py $ S.elems cubes
        maxY = maximum $ map py $ S.elems cubes
        minZ = minimum $ map pz $ S.elems cubes
        maxZ = maximum $ map pz $ S.elems cubes
        bounds = ((minX-1, minY-1, minZ-1), (maxX+1, maxY+1, maxZ+1))
    print $ sum $ map (length . filter (`S.notMember` cubes) . adjacents) (S.elems cubes)
    print $ sum $ map (length . filter (\adj -> adj `S.notMember` cubes && hasPath bounds cubes adj) . adjacents)
         $ filter (\p -> hasPath bounds cubes p) $ S.elems cubes

-- TODO: build up one array of which points are reachable from outside isntead of recalculating
-- reachability per adjacent per point
