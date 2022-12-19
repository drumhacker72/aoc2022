import Data.Bifunctor (first, second)
import Data.Functor (void)
import Data.List (foldl')
import Data.Maybe (fromJust, maybeToList)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseMaybe, sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char (char, space1, string)

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Resource = Clay | Geode | Obsidian | Ore
    deriving (Enum, Eq, Ord, Show)

data ResourceMap a = ResourceMap !a !a !a !a
    deriving Show

fromList :: a -> [(Resource, a)] -> ResourceMap a
fromList e [] = ResourceMap e e e e
fromList e ((Clay, a):xs) = let ResourceMap _ b c d = fromList e xs in ResourceMap a b c d
fromList e ((Geode, b):xs) = let ResourceMap a _ c d = fromList e xs in ResourceMap a b c d
fromList e ((Obsidian, c):xs) = let ResourceMap a b _ d = fromList e xs in ResourceMap a b c d
fromList e ((Ore, d):xs) = let ResourceMap a b c _ = fromList e xs in ResourceMap a b c d

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

rsrcSequence :: Monad m => ResourceMap (m a) -> m (ResourceMap a)
rsrcSequence (ResourceMap a b c d) = do
    a' <- a
    b' <- b
    c' <- c
    d' <- d
    return $ ResourceMap a' b' c' d'

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
    pure (robot, fromList 0 ingredients)

type Blueprint = ResourceMap (ResourceMap Int)

pBlueprint :: Parser (Int, Blueprint)
pBlueprint = do
    void $ string "Blueprint "
    idNum <- L.decimal
    void $ char ':'
    space1
    recipes <- sepEndBy1 pRecipe space1
    pure (idNum, fromJust $ rsrcSequence $ fromList Nothing (second Just <$> recipes))

data State = State
    { resources :: !(ResourceMap Int)
    , robots :: !(ResourceMap Int)
    } deriving Show

gather :: State -> ResourceMap Int
gather (State rs bs) = rsrcZipWith (+) rs bs

tryMakeRobot :: (Resource, ResourceMap Int) -> State -> Maybe (State, ResourceMap Int)
tryMakeRobot (robot, ingredients) s@(State rs bs) =
    if rsrcFoldr (&&) True (rsrcZipWith (>=) rs ingredients) then Just (s', bs') else Nothing
  where
    s' = s { resources = rsrcZipWith (-) rs ingredients }
    bs' = adjust (+ 1) robot bs

membersGTE :: ResourceMap Int -> ResourceMap Int -> Bool
membersGTE (ResourceMap a1 b1 c1 d1) (ResourceMap a2 b2 c2 d2) = a1 >= a2 && b1 >= b2 && c1 >= c2 && d1 >= d2

isBetterThan :: Int -> Blueprint -> State -> State -> Bool
isBetterThan _t _bp (State r1 b1) (State r2 b2) = r1 `membersGTE` r2 && b1 `membersGTE` b2

step :: Int -> Blueprint -> [State] -> [State]
step t bp states =
    let builds = concatMap (\s@(State _ bs) ->
            case tryMakeRobot (Geode, bp ! Geode) s of
                Just x -> [x]
                Nothing -> case tryMakeRobot (Obsidian, bp ! Obsidian) s of
                    Just x -> [x]
                    Nothing -> (s, bs) : concatMap (\r -> maybeToList $ tryMakeRobot (r, bp ! r) s) [Clay, Ore]
            ) states
        gathers = map (uncurry State . first gather) builds
     in foldl' (\acc s -> (if any (\x -> isBetterThan t bp x s) acc then filter (not . (isBetterThan t bp s)) acc else s:acc))
               [] gathers

loop :: Int -> Blueprint -> [State] -> [[State]]
loop 0 _ _ = []
loop t bp states = let states' = step t bp states in states' : loop (t-1) bp states'

numGeodes :: State -> Int
numGeodes (State rs _) = rs ! Geode

main :: IO ()
main = do
    contents <- readFile "day19.txt"
    let blueprints = maybe (error "bad input") id $ parseMaybe (some pBlueprint) contents
    let initial = State (fromList 0 []) (fromList 0 [(Ore, 1)])

    let qualityLevels = map (\(idNum, bp) -> idNum * maximum (map numGeodes $ last $ loop 24 bp [initial])) blueprints
    print $ sum qualityLevels

    print $ product $ map (\(_, bp) -> maximum (map numGeodes $ last $ loop 32 bp [initial])) $ take 3 blueprints
