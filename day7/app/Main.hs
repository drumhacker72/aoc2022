{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isLower)
import Data.Functor ((<&>), void)
import Data.List (sort)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Encoding (decodeASCII)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), eof, parseMaybe, sepEndBy1, some, takeWhile1P)
import Text.Megaparsec.Char (char, eol, string)

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Destination
    = InDest Text
    | OutDest
    | RootDest
    deriving Show

data Result
    = DirResult Text
    | FileResult Int Text
    deriving Show

data Command
    = CdCmd Destination
    | LsCmd [Result]
    deriving Show

destination :: Parser Destination
destination =
    (string ".." *> pure OutDest)
    <|> (char '/' *> pure RootDest)
    <|> (takeWhile1P (Just "dirname character") isLower <&> InDest)

result :: Parser Result
result =
    (string "dir " *> takeWhile1P (Just "dirname character") isLower <&> DirResult)
    <|> (do
        size <- L.decimal
        void $ char ' '
        name <- takeWhile1P (Just "filename character") (\c -> c == '.' || isLower c)
        pure $ FileResult size name)

command :: Parser Command
command = string "$ " *> (
    (string "cd " *> destination <* (void eol <|> eof) <&> CdCmd)
    <|> (string "ls" *> eol *> sepEndBy1 result eol <&> LsCmd))

input :: Parser [Command]
input = some command

data Directory = Directory
    { subdirs :: Map Text Directory
    , files :: Map Text Int
    } deriving Show

descend :: Text -> (Directory, [Directory -> Directory]) -> (Directory, [Directory -> Directory])
descend subname (dir, parents) = (subdir, parents')
  where
    subdir = M.findWithDefault (Directory M.empty M.empty) subname (subdirs dir)
    parents' = (\subdir' -> dir { subdirs = M.insert subname subdir' (subdirs dir) }) : parents

ascend :: (Directory, [Directory -> Directory]) -> (Directory, [Directory -> Directory])
ascend (_, []) = error "cannot ascend from root"
ascend (dir, p:parents) = (p dir, parents)

ascendAll :: (Directory, [Directory -> Directory]) -> Directory
ascendAll (dir, []) = dir
ascendAll dp = ascendAll (ascend dp)

loop :: [Command] -> (Directory, [Directory -> Directory]) -> (Directory, [Directory -> Directory])
loop [] dp = dp
loop (c:cs) dp@(dir, parents) = loop cs dp'
  where
    dp' = case c of
        CdCmd (InDest subname) -> descend subname dp
        CdCmd OutDest -> ascend dp
        CdCmd RootDest -> (ascendAll dp, [])
        LsCmd results -> (loop' results dir, parents)

loop' :: [Result] -> Directory -> Directory
loop' [] d = d
loop' (r:rs) d = loop' rs d'
  where
    d' = case r of
        DirResult _ -> d
        FileResult s f -> d { files = M.insert f s (files d) }

totalSize :: Directory -> Int
totalSize (Directory subdirs files) = sum (map totalSize $ M.elems subdirs) + sum (M.elems files)

allSizes :: Directory -> [Int]
allSizes d@(Directory subdirs _) = totalSize d : concatMap allSizes subdirs

main :: IO ()
main = do
    contents <- decodeASCII <$> B.readFile "day7.txt"
    let commands = maybe (error "bad input") id $ parseMaybe input contents
        root = Directory M.empty M.empty
        root' = ascendAll $ loop commands (root, [])
    print $ sum $ filter (<= 100000) $ allSizes root'
    let threshold = 30_000_000 - (70_000_000 - totalSize root')
    print $ head $ sort $ filter (>= threshold) $ allSizes root'
