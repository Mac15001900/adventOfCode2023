import           Data.List
import           Data.Maybe

import           MUtils

type Hand = [Int] --Storing hands as lists of integers that represent the 'strength' of each card

parseLine :: String -> String -> (Hand, Int)
parseLine order line = break (== ' ') line |> mapFst (map (\card -> elemIndex card order |> fromJust)) |> mapSnd read

handAmounts :: Hand -> [Int] --How many of each type of card are there is a hand
handAmounts [] = []
handAmounts (x : xs) = count (== x) xs + 1 : handAmounts (filter (/= x) xs)

handRank :: Hand -> Int
handRank h = handAmounts h |> sort |> reverse |> take 2 |> \(h : r) -> (10 * h : r) |> sum

compareHands :: (Hand -> Int) -> (Hand, Int) -> (Hand, Int) -> Ordering
compareHands ranker (h1, _) (h2, _) = if rankCompare /= EQ then rankCompare else compare h1 h2
  where
    rankCompare = compare (ranker h1) (ranker h2)

part1 :: [String] -> Int
part1 lines = map (parseLine "23456789TJQKA") lines |> sortBy (compareHands handRank) |> map snd |> (0 :) |> zipWithIndexes
    |> map (uncurry (*)) |> sum

handRank2 :: Hand -> Int
handRank2 h = [ 1 .. 12 ] |> map (\j -> map (\c -> if c == 0 then j else c) h) |> map handRank |> maximum

part2 :: [String] -> Int
part2 lines = map (parseLine "J23456789TQKA") lines |> sortBy (compareHands handRank2) |> map snd |> (0 :) |> zipWithIndexes
    |> map (uncurry (*)) |> sum

test = [ "32T3K 765", "T55J5 684", "KK677 28", "KTJJT 220", "QQQJA 483" ]