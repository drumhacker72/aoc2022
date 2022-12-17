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

simulate :: Int -> Map Text (Int, [Text]) -> Map Text [(Set Text, Int)] -> Int
simulate 30 _ states = maximum $ concatMap (map snd) $ M.elems states
simulate time rooms states = simulate (time+1) rooms $ foldl' f M.empty (M.assocs states)
  where
    f :: Map Text [(Set Text, Int)] -> (Text, [(Set Text, Int)]) -> Map Text [(Set Text, Int)]
    f acc (room, ops) =
        let accv = foldl' (h room) acc ops
         in foldl' (g ops) accv $ snd $ rooms M.! room
    h room acc' (open, pressure) =
        let pressure' = pressure + sum (fst . (rooms M.!) <$> S.elems open)
            rate = fst $ rooms M.! room
            in if rate == 0 || room `S.member` open then acc' else try room (S.insert room open) pressure' acc'
    try :: Text -> Set Text -> Int -> Map Text [(Set Text, Int)] -> Map Text [(Set Text, Int)]
    try r o' p' acc' = case M.lookup r acc' of
        Nothing -> M.insert r [(o', p')] acc'
        Just oldops ->
            -- TODO: instead of subsets, compare combined rate of all open valves?
            let ops' = filter (\(o, p) -> p > p' || not (o `S.isSubsetOf` o')) oldops
                ops'' = if any (\(o, p) -> p > p' && o' `S.isSubsetOf` o) oldops then oldops else (o', p') : ops'
                in M.insert r ops'' acc'
    g :: [(Set Text, Int)] -> Map Text [(Set Text, Int)] -> Text -> Map Text [(Set Text, Int)]
    g ops acc' r' = foldl' (j r') acc' ops
    j r' acc' (open, pressure) =
        let pressure' = pressure + sum (fst . (rooms M.!) <$> S.elems open)
            in try r' open pressure' acc'

main :: IO ()
main = do
    contents <- decodeASCII <$> B.readFile "day16.txt"
    let report = maybe (error "bad input") id $ parseMaybe (sepEndBy1 pRoom eol) contents
        rooms = M.fromList report
    print $ simulate 0 rooms $ M.singleton "AA" [(S.empty, 0)]
