import           Data.Char
import           Data.List
import           Data.Maybe

import           MUtils

import           Text.Regex

part1 :: [String] -> Int
part1 lines = lines |> map (\x -> filter isDigit x |> map singleton |> map read) |> map (\xs -> (head xs) * 10 + (last xs)) |> sum

digits :: [String]
digits = [ "-", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]

part2Wrong :: [String] -> [String]
part2Wrong lines = map replacer lines
  where
    fs = digits |> zipWithIndexes |> reverse |> map (\(s, i) -> \x -> subRegex (mkRegex s) x (show i))
    replacer = foldl (.) id fs

findFirstDigit :: String -> [String] -> Maybe Int
findFirstDigit [] _ = Nothing
findFirstDigit s@(x : xs) digitStrings
    | isDigit x = Just (read [ x ])
    | not (null fromText) = Just (head fromText)
    | otherwise = findFirstDigit xs digitStrings
  where
    checkFor digit = length s >= length digit && take (length digit) s == digit
    fromText = map checkFor digitStrings |> zipWithIndexes |> filter fst |> map snd

part2 :: [String] -> Int
part2 lines = map (\line -> (findFirstDigit line digits |> fromJust) * 10 + (findFirstDigit (reverse line) (map reverse digits) |> fromJust)) lines |> sum

test = [ "two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen" ]