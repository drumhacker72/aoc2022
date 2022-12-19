import Control.Monad (forM_)
import Data.Bifunctor (first, second)
import Data.Functor (void)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseMaybe, sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char (char, space1, string)

import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Resource = Clay | Geode | Obsidian | Ore
    deriving (Enum, Eq, Ord, Show)

data ResourceMap a = ResourceMap !a !a !a !a
    deriving Show

fromList :: [(Resource, Int)] -> ResourceMap Int
fromList [] = ResourceMap 0 0 0 0
fromList ((Clay, a):xs) = let ResourceMap _ b c d = fromList xs in ResourceMap a b c d
fromList ((Geode, b):xs) = let ResourceMap a _ c d = fromList xs in ResourceMap a b c d
fromList ((Obsidian, c):xs) = let ResourceMap a b _ d = fromList xs in ResourceMap a b c d
fromList ((Ore, d):xs) = let ResourceMap a b c _ = fromList xs in ResourceMap a b c d

adjust :: (a -> a) -> Resource -> ResourceMap a -> ResourceMap a
adjust f k (ResourceMap a b c d) = case k of
    Clay -> ResourceMap (f a) b c d
    Geode -> ResourceMap a (f b) c d
    Obsidian -> ResourceMap a b (f c) d
    Ore -> ResourceMap a b c (f d)

(!) :: ResourceMap a -> Resource -> a
ResourceMap a b c d ! k = case k of
    Clay -> a
    Geode -> b
    Obsidian -> c
    Ore -> d

rsrcZipWith :: (a -> b -> c) -> ResourceMap a -> ResourceMap b -> ResourceMap c
rsrcZipWith f (ResourceMap a1 b1 c1 d1) (ResourceMap a2 b2 c2 d2) = ResourceMap (f a1 a2) (f b1 b2) (f c1 c2) (f d1 d2)

rsrcFoldr :: (a -> b -> b) -> b -> ResourceMap a -> b
rsrcFoldr f x (ResourceMap a b c d) = f a (f b (f c (f d x)))

pResource :: Parser Resource
pResource =
    (string "clay" *> pure Clay)
    <|> (string "geode" *> pure Geode)
    <|> (string "obsidian" *> pure Obsidian)
    <|> (string "ore" *> pure Ore)

pResourceAmount :: Parser (Resource, Int)
pResourceAmount = do
    num <- L.decimal
    void $ char ' '
    rsrc <- pResource
    pure (rsrc, num)

type Recipe = (Resource, ResourceMap Int)

pRecipe :: Parser Recipe
pRecipe = do
    void $ string "Each "
    robot <- pResource
    void $ string " robot costs "
    ingredients <- sepBy1 pResourceAmount (string " and ")
    void $ char '.'
    pure (robot, fromList ingredients)

type Blueprint = Map Resource (ResourceMap Int)

pBlueprint :: Parser (Int, Blueprint)
pBlueprint = do
    void $ string "Blueprint "
    idNum <- L.decimal
    void $ char ':'
    space1
    recipes <- sepEndBy1 pRecipe space1
    pure (idNum, M.fromList recipes)

data State = State
    { resources :: !(ResourceMap Int)
    , robots :: !(ResourceMap Int)
    } deriving Show

gather :: State -> ResourceMap Int
gather (State rs bs) = rsrcZipWith (+) rs bs

tryMakeRobot :: (Resource, ResourceMap Int) -> State -> [(State, ResourceMap Int)]
tryMakeRobot (robot, ingredients) s@(State rs bs) =
    if rsrcFoldr (&&) True (rsrcZipWith (>=) rs ingredients) then [(s', bs')] else []
  where
    s' = s { resources = rsrcZipWith (-) rs ingredients }
    bs' = adjust (+ 1) robot bs

membersGTE :: ResourceMap Int -> ResourceMap Int -> Bool
membersGTE (ResourceMap a1 b1 c1 d1) (ResourceMap a2 b2 c2 d2) = a1 >= a2 && b1 >= b2 && c1 >= c2 && d1 >= d2

isBetterThan :: Int -> Blueprint -> State -> State -> Bool
isBetterThan t bp (State r1 b1) (State r2 b2) =
    (r1 `membersGTE` r2 && b1 `membersGTE` b2)
    || (r1 ! Obsidian + t * (b1 ! Obsidian) > r2 ! Obsidian + t * (b2 ! Obsidian)
        && r1 ! Geode + t * (b1 ! Geode) > r2 ! Geode + t * (b2 ! Geode))

step :: Int -> Blueprint -> [State] -> [State]
step t bp states =
    let builds = concatMap (\s@(State _ bs) -> (s, bs) : concatMap (\r -> tryMakeRobot r s) (M.assocs bp)) states
        gathers = map (uncurry State . first gather) builds
     in foldl' (\acc s -> (if any (\x -> isBetterThan t bp x s) acc then filter (not . (isBetterThan t bp s)) acc else s:acc))
               [] gathers

loop :: Int -> Blueprint -> [State] -> [[State]]
loop 0 _ states = []
loop t bp states = let states' = step t bp states in states' : loop (t-1) bp states'

numGeodes :: State -> Int
numGeodes (State rs _) = rs ! Geode

main :: IO ()
main = do
    contents <- readFile "day19.txt"
    let blueprints = maybe (error "bad input") id $ parseMaybe (some pBlueprint) contents
    let initial = State (fromList []) (fromList [(Ore, 1)])

    -- forM_ blueprints $ \(_, bp) ->  mapM (print . second length) $ zip [1::Int ..] $ loop 24 bp [initial]

    let qualityLevels = map (\(idNum, bp) -> idNum * maximum (map numGeodes $ last $ loop 24 bp [initial])) blueprints
    print $ sum qualityLevels
