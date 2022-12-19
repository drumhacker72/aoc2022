import Data.Functor (void)
import Data.Set (Set)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepEndBy1)
import Text.Megaparsec.Char (char, eol)

import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pPosition :: Parser (Int, Int, Int)
pPosition = do
    x <- L.decimal
    void $ char ','
    y <- L.decimal
    void $ char ','
    z <- L.decimal
    pure (x, y, z)

adjacents :: (Int, Int, Int) -> [(Int, Int, Int)]
adjacents (x, y, z) =
    [ (x-1, y, z), (x+1, y, z)
    , (x, y-1, z), (x, y+1, z)
    , (x, y, z-1), (x, y, z+1)
    ]

main :: IO ()
main = do
    contents <- readFile "day18.txt"
    let cubes = S.fromList $ maybe (error "bad input") id $ parseMaybe (sepEndBy1 pPosition eol) contents
    print $ sum $ map (length . filter (`S.notMember` cubes) . adjacents) (S.elems cubes)
