import Data.Foldable (toList)
import Data.Functor (void)
import Data.List (foldl', transpose)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseMaybe, sepBy1, sepEndBy1)
import Text.Megaparsec.Char (char, digitChar, eol, string, upperChar)

import qualified Data.Sequence as S
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Step = Step
    { count :: Int
    , from :: Int
    , to :: Int
    } deriving Show

crate :: Parser (Maybe Char)
crate = (Just <$> (char '[' *> upperChar <* char ']')) <|> (string "   " *> pure Nothing)

crateLayer :: Parser [Maybe Char]
crateLayer = sepBy1 crate (char ' ')

label :: Parser ()
label = char ' ' *> digitChar *> char ' ' *> pure ()

labelRow :: Parser ()
labelRow = sepBy1 label (char ' ') *> pure ()

crateLayers :: Parser [[Maybe Char]]
crateLayers = sepEndBy1 crateLayer eol <* labelRow <* eol

step :: Parser Step
step = do
    void $ string "move "
    count <- L.decimal
    void $ string " from "
    from <- L.decimal
    void $ string " to "
    to <- L.decimal
    pure $ Step count from to

input :: Parser ([[Maybe Char]], [Step])
input = do
    layers <- crateLayers
    void eol
    steps <- sepEndBy1 step eol
    pure (layers, steps)

runStep1 :: Seq [Char] -> Step -> Seq [Char]
runStep1 stacks (Step 0 _ _) = stacks
runStep1 stacks (Step count from to) = runStep1 stacks' $ Step (count-1) from to
  where
    (pickedCrate, rest) = case stacks `S.index` (from-1) of
        (c:cs) -> (c, cs)
        [] -> error "cannot pull from empty stack"
    stacks' = S.adjust' (pickedCrate:) (to-1) $ S.update (from-1) rest stacks

runStep2 :: Seq [Char] -> Step -> Seq [Char]
runStep2 stacks (Step count from to) =
    S.adjust' (pickedCrates ++) (to-1) $ S.update (from-1) rest stacks
  where
    (pickedCrates, rest) = splitAt count (stacks `S.index` (from-1))

main :: IO ()
main = do
    contents <- readFile "day5.txt"
    let (layers, steps) = maybe (error "bad input") id $ parseMaybe input contents
    let stacks = S.fromList . map catMaybes $ transpose layers
    putStrLn $ map head $ toList $ foldl' runStep1 stacks steps
    putStrLn $ map head $ toList $ foldl' runStep2 stacks steps
