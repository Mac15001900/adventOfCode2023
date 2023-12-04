import qualified Data.Set as Set

import           MUtils

type Card = (Set.Set Int, Set.Set Int)

parseCard :: String -> Card
parseCard line = splitOn ':' line |> last |> splitOn '|' |> map (splitOn ' ') |> filter2 (not . null) |> map2 readInt |> map Set.fromList |> t2fromList

part1 :: [String] -> Int
part1 lines = map parseCard lines |> map (uncurry Set.intersection) |> map length |> map (subtract 1) |> map (2 ^^) |> map floor |> sum

part2 :: [String] -> Int
part2 lines = map parseCard lines |> map (uncurry Set.intersection) |> map length |> map (1, ) |> countCards

countCards :: [(Int,Int)] -> Int --For each scratchcard, we have an amount and how many below does it win
countCards [] = 0
countCards ((n,w):xs) = n + countCards ((take w xs |> map (mapFst (+n))) ++ drop w xs)

test = [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
       , "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
       , "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
       , "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
       , "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
       , "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
       ]