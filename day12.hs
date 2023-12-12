{-# LANGUAGE TupleSections #-}

import           Data.Tuple

import           MUtils

data SpringData = Broken Int | Working
    deriving ( Show, Read, Eq, Ord )

type State = ([SpringData], String)

makeSpringData :: String -> [SpringData]
makeSpringData lineEnd = splitOn ',' lineEnd |> map read |> map Broken |> map (Working, ) |> map t2toList |> concat

-- countOptions :: [SpringData] -> String -> Integer
countOptions :: State -> Either [State] Integer
countOptions ([], s) = if '#' `elem` s then Right 0 else Right 1 --Make sure there are no more broken springs
countOptions ([Broken 0], []) = Right 1
countOptions (_, []) = Right 0
countOptions (sd, s)
    | minLength sd > 1 + length s = Right 0 --Stopping early if we don't have enough springs left to match the data
countOptions (Broken 0 : sd, '#' : s) = Right 0 --Contradiction: this spring should be working, but it's broken
countOptions (Broken 0 : sd, _ : s) = Left [ (sd, s) ]
countOptions (Broken n : sd, '.' : _) = Right 0 --Contradition: this spring should be broken, but it's working
countOptions (Broken n : sd, _ : s) = Left [ (Broken (n - 1) : sd, s) ]
countOptions (Working : sd, '.' : s) = Left [ (Working : sd, s) ]
countOptions (Working : sd, '#' : s) = Left [ (sd, '#' : s) ]
countOptions (Working : sd, '?' : s) = Left [ (sd, '?' : s), (Working : sd, s) ]
countOptions (ds, s) = error (show ds ++ " | " ++ s)

minLength :: [SpringData] -> Int
minLength [] = 0
minLength (Working : sd) = 1 + minLength sd
minLength ((Broken n) : sd) = n + minLength sd

part1 :: [String] -> Integer
part1 lines = map (splitOn ' ') lines |> map t2fromList |> map (mapSnd makeSpringData) |> map swap |> map (memoizedCount countOptions) |> sum

part2 :: [String] -> Integer
part2 lines = map (splitOn ' ') lines |> map2 (replicate 5) |> map t2fromList |> map (mapFst (joinWith "?"))
    |> map (mapSnd (joinWith ",")) |> map t2toList |> map (joinWith " ") |> part1

test = [ "???.### 1,1,3", ".??..??...?##. 1,1,3", "?#?#?#?#?#?#?#? 1,3,1,6", "????.#...#... 4,1,1", "????.######..#####. 1,6,5", "?###???????? 3,2,1" ]