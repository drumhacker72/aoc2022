import Data.Set (Set)

import qualified Data.Set as S

data Jet = L | R
    deriving (Enum, Show)

toJet :: Char -> Jet
toJet '<' = L
toJet '>' = R
toJet c = error $ "unexpected character: " <> [c]

type Rock = [(Int, Int)]

rocks :: [Rock]
rocks =
    [ [(0, 0), (1, 0), (2, 0), (3, 0)]         -- -
    , [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)] -- +
    , [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)] -- ⌟
    , [(0, 0), (0, 1), (0, 2), (0, 3)]         -- |
    , [(0, 0), (1, 0), (0, 1), (1, 1)]         -- □
    ]

rockIdxs :: Rock -> (Int, Int) -> [(Int, Int)]
rockIdxs rock (x, y) = map (\(dx, dy) -> (x + dx, y + dy)) rock

intersection :: Rock -> (Int, Int) -> Set (Int, Int) -> Bool
intersection rock pos grid = any (`S.member` grid) $ rockIdxs rock pos

inBounds :: Rock -> (Int, Int) -> Bool
inBounds rock pos = minimum (map fst $ rockIdxs rock pos) >= 1 && maximum (map fst $ rockIdxs rock pos) <= 7

dropRock :: Int -> Int -> Rock -> [Jet] -> (Int, Int) -> Set (Int, Int) -> (Int, Int, Set (Int, Int))
dropRock numJets height rock (jet:js) pos@(x, y) grid =
    let jetpos = case jet of
            L -> (x-1, y)
            R -> (x+1, y)
        pos'@(x', y') = if intersection rock jetpos grid || not (inBounds rock jetpos) then pos else jetpos
        droppos = (x', y' - 1)
     in if y' == 1 || intersection rock droppos grid
        then let grid' = (S.union grid $ S.fromList $ rockIdxs rock pos')
                 height' = max height (maximum $ map snd $ rockIdxs rock pos')
              in (numJets+1, height', grid')
        else dropRock (numJets+1) height rock js droppos grid
dropRock _ _ _ _ _ _ = error "ran out of rocks or jets"

dropAllRocks :: Int -> Int -> [Rock] -> [Jet] -> Set (Int, Int) -> (Int, Int, Set (Int, Int))
dropAllRocks numJets height [] _ grid = (numJets, height, grid)
dropAllRocks numJets height (rock:rs) js grid =
    let (numJets', height', grid') = dropRock numJets height rock js (3, height + 4) grid
     in dropAllRocks numJets' height' rs (drop (numJets' - numJets) js) grid'

jetCycle :: Int -> Int -> Int -> [Jet] -> Set (Int, Int) -> [Int] -> [Int]
jetCycle 0 _ _ _ _ seenJetNums = seenJetNums
jetCycle n numJets height jets grid seenJetNums =
    let (numJets', height', grid') = dropAllRocks numJets height rocks (drop numJets $ cycle jets) grid
        nj' = numJets' `mod` length jets
     in jetCycle (n-1) nj' height' jets grid' (numJets:seenJetNums)

simulate :: Int -> Int -> [Rock] -> [Jet] -> (Int, Int) -> Set (Int, Int) -> (Int, Set (Int, Int))
simulate 0 height _ _ _ grid = (height, grid)
simulate n height (rock:rs) (jet:js) pos@(x, y) grid =
    let jetpos = case jet of
            L -> (x-1, y)
            R -> (x+1, y)
        pos'@(x', y') = if intersection rock jetpos grid || not (inBounds rock jetpos) then pos else jetpos
        droppos = (x', y' - 1)
     in if y' == 1 || intersection rock droppos grid
        then let grid' = (S.union grid $ S.fromList $ rockIdxs rock pos')
                 height' = max height (maximum $ map snd $ rockIdxs rock pos')
              in simulate (n-1) height' rs js (3, height' + 4) grid'
        else simulate n height (rock:rs) js droppos grid
simulate _ _ _ _ _ _ = error "ran out of rocks or jets"

main :: IO ()
main = do
    contents <- readFile "day17.txt"
    let jets = case lines contents of [l] -> map toJet l; _ -> error "expected only one line"

    let sim n = fst $ simulate n 0 (cycle rocks) (cycle jets) (3, 4) S.empty
    print $ sim 2022

    -- print $ jetCycle 999 0 0 jets S.empty [] -- dump the jet cycle for manual inspection

    -- let (start, cycleLen) = findJetCycle 0 0 jets S.empty []
    let (start, cycleLen) = (42, 348) -- TODO: find programmatically, these were found by hand for the given input
    -- let (start, cycleLen) = (3, 7) -- found by hand for the example input
        rstart = start * length rocks
        rcycle = cycleLen * length rocks
        hstart = sim rstart
        hcycle = sim (rstart + rcycle) - hstart
        (d, m) = (1000000000000 - rstart) `divMod` rcycle
        hend = sim (rstart + m) - hstart
    print $ hstart + d * hcycle + hend
