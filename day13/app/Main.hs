import Data.List (elemIndex, sort)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseMaybe, sepBy, sepEndBy1)
import Text.Megaparsec.Char (char, eol)

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Value = IntValue Int | ListValue [Value]
    deriving (Eq, Show)

instance Ord Value where
    compare (IntValue a) (IntValue b) = compare a b
    compare (ListValue []) (ListValue []) = EQ
    compare (ListValue []) (ListValue _) = LT
    compare (ListValue _) (ListValue []) = GT
    compare (ListValue (a:as)) (ListValue (b:bs)) = case compare a b of
        EQ -> compare (ListValue as) (ListValue bs)
        r -> r
    compare a@(IntValue _) bs@(ListValue _) = compare (ListValue [a]) bs
    compare as@(ListValue _) b@(IntValue _) = compare as (ListValue [b])

pList :: Parser [Value]
pList = char '[' *> sepBy pValue (char ',') <* char ']'

pValue :: Parser Value
pValue = fmap ListValue pList <|> fmap IntValue L.decimal

pPacketPair :: Parser ([Value], [Value])
pPacketPair = do
    [left, right] <- sepEndBy1 pList eol
    pure (left, right)

pInput :: Parser [([Value], [Value])]
pInput = sepEndBy1 pPacketPair eol

main :: IO ()
main = do
    contents <- readFile "day13.txt"
    let packetPairs = maybe (error "bad input") id $ parseMaybe pInput contents
    print $ sum $ map fst $ filter (\(_, o) -> o /= GT) $ zip [1::Int ..] $ map (uncurry compare) packetPairs

    let div1 = [ListValue [IntValue 2]]
        div2 = [ListValue [IntValue 6]]
        allPackets = sort $ div1 : div2 : concatMap (\(l, r) -> [l, r]) packetPairs
        (i1, i2) = case (elemIndex div1 allPackets, elemIndex div2 allPackets) of
            (Just a, Just b) -> (a, b)
            _ -> error "failed to find divider packets"
    print $ (i1 + 1) * (i2 + 1)
