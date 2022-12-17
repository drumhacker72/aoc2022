{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isUpper)
import Data.Functor ((<&>), void)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding (decodeASCII)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseMaybe, sepBy1, sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (eol, string)

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

pValve :: Parser Text
pValve = takeWhile1P (Just "valve letter") isUpper

pRoom :: Parser (Text, (Int, [Text]))
pRoom = do
    void $ string "Valve "
    name <- pValve
    void $ string " has flow rate="
    rate <- L.decimal
    tunnels <- (string "; tunnels lead to valves " *> sepBy1 pValve (string ", "))
           <|> (string "; tunnel leads to valve " *> pValve <&> (:[]))
    pure (name, (rate, tunnels))

type Layout = Map Text (Int, [Text])

openRate :: Layout -> Set Text -> Int
openRate rooms open = sum (fst . (rooms M.!) <$> S.elems open)

try :: Ord k => Layout -> k -> Set Text -> Int -> Map k [(Set Text, Int)] -> Map k [(Set Text, Int)]
try rooms k o' p' acc' = case M.lookup k acc' of
    Nothing -> M.insert k [(o', p')] acc'
    Just ops ->
        let ops' = (o', p') : filter (\(o, p) -> p > p' || openRate rooms o > openRate rooms o') ops
         in M.insert k ops' acc'

simulate :: Int -> Layout -> Map Text [(Set Text, Int)] -> Int
simulate 30 _ states = maximum $ concatMap (map snd) $ M.elems states
simulate time rooms states = simulate (time+1) rooms $ foldl' processRoom M.empty (M.assocs states)
  where
    processRoom :: Map Text [(Set Text, Int)] -> (Text, [(Set Text, Int)]) -> Map Text [(Set Text, Int)]
    processRoom acc (room, ops) =
        let acc' = foldl' (tryOpeningValve room) acc ops
         in foldl' (tryAllMoves ops) acc' $ snd $ rooms M.! room

    tryOpeningValve :: Text -> Map Text [(Set Text, Int)] -> (Set Text, Int) -> Map Text [(Set Text, Int)]
    tryOpeningValve room acc' (open, pressure) =
        let pressure' = pressure + openRate rooms open
            rate = fst $ rooms M.! room
            in if rate == 0 || room `S.member` open then acc' else try rooms room (S.insert room open) pressure' acc'

    tryAllMoves :: [(Set Text, Int)] -> Map Text [(Set Text, Int)] -> Text -> Map Text [(Set Text, Int)]
    tryAllMoves ops acc' r' = foldl' (tryMove r') acc' ops

    tryMove :: Text -> Map Text [(Set Text, Int)] -> (Set Text, Int) -> Map Text [(Set Text, Int)]
    tryMove r' acc' (open, pressure) =
        let pressure' = pressure + openRate rooms open
            in try rooms r' open pressure' acc'

data Action = Open | Move | None

simulate2 :: Int -> Layout -> Map (Text, Text) [(Set Text, Int)] -> Int
simulate2 26 _ states = maximum $ concatMap (map snd) $ M.elems states
simulate2 time rooms states = simulate2 (time+1) rooms $ foldl' processRoom M.empty (M.assocs states)
  where
    processRoom :: Map (Text, Text) [(Set Text, Int)] -> ((Text, Text), [(Set Text, Int)]) -> Map (Text, Text) [(Set Text, Int)]
    processRoom acc ((room1, room2), ops) =
        let possibilities = concatMap (\(o, p) -> do
                    let p' = p + openRate rooms o
                    (a1, o') <- tryOpeningValve room1 o
                    (a2, o'') <- tryOpeningValve room2 o'
                    r1' <- case a1 of
                        None -> ["AA"] -- hack: collapse all paths when done opening valves
                        Move -> snd $ rooms M.! room1
                        Open -> [room1]
                    r2' <- case a2 of
                        None -> ["AA"]
                        Move -> snd $ rooms M.! room2
                        Open -> [room2]
                    [((r1', r2'), (o'', p'))]
                ) ops
         in foldl' (\acc' (k, (o, p)) -> try rooms k o p acc') acc possibilities

    tryOpeningValve :: Text -> Set Text -> [(Action, Set Text)]
    tryOpeningValve room open =
        let numValves = filter (\(r, _) -> r /= 0) $ M.elems rooms
            rate = fst $ rooms M.! room
         in (if length open == length numValves then None else Move, open)
            : if rate == 0 || room `S.member` open then [] else [(Open, S.insert room open)]

main :: IO ()
main = do
    contents <- decodeASCII <$> B.readFile "day16.txt"
    let report = maybe (error "bad input") id $ parseMaybe (sepEndBy1 pRoom eol) contents
        rooms = M.fromList report
    print $ simulate 0 rooms $ M.singleton "AA" [(S.empty, 0)]
    print $ simulate2 0 rooms $ M.singleton ("AA", "AA") [(S.empty, 0)]
