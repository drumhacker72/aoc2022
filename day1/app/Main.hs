import Data.List (sort)
import Data.Text (Text)
import Data.Text.Encoding (decodeASCII)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepBy1, sepEndBy1)
import Text.Megaparsec.Char (eol)

import qualified Data.ByteString as B
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

inventory :: Parser [Int]
inventory = sepEndBy1 L.decimal eol

input :: Parser [[Int]]
input = sepBy1 inventory eol

main :: IO ()
main = do
    contents <- decodeASCII <$> B.readFile "day1.txt"
    let elves = maybe (error "bad input") id $ parseMaybe input contents
    print $ maximum $ map sum elves
    print $ sum $ take 3 $ reverse $ sort $ map sum elves
