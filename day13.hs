import           Data.List
import           Data.Maybe ( fromMaybe )

import           MUtils

reflectionRow :: Eq a => [a] -> Int
reflectionRow xs = allReflections xs |> (!!? 0) |> fromMaybe 0

part1 :: [String] -> Int
part1 lines = 100 * (splitOn "" lines |> map reflectionRow |> sum) + (splitOn "" lines |> map transpose |> map reflectionRow |> sum)

smudges :: [String] -> [[String]]
smudges xs = indexes2 xs |> map (\(x, y) -> changeElement2 x y flipSpot xs)

flipSpot :: Char -> Char
flipSpot '.' = '#'
flipSpot '#' = '.'

allReflections :: Eq a => [a] -> [Int]
allReflections xs = [ 1 .. length xs - 1 ] |> filter (\n -> zip (take n xs |> reverse) (drop n xs) |> map (uncurry (==)) |> and)

findNewReflection :: Eq a => Int -> [[a]] -> Int --Finds a reflection that's different than the given old reflection within a list of areas
findNewReflection old xs = map allReflections xs |> map (filter (/= old)) |> map (!!? 0) |> removeNothing |> (!!? 0) |> fromMaybe 0

processArea :: [String] -> Int
processArea area = 100 * findNewReflection oldReflectionH (smudges area) + findNewReflection oldReflectionV (transpose area |> smudges)
  where
    oldReflectionH = reflectionRow area
    oldReflectionV = reflectionRow (transpose area)

part2 :: [String] -> Int
part2 lines = splitOn "" lines |> map processArea |> sum

test = [ "#.##..##."
       , "..#.##.#."
       , "##......#"
       , "##......#"
       , "..#.##.#."
       , "..##..##."
       , "#.#.##.#."
       , ""
       , "#...##..#"
       , "#....#..#"
       , "..##..###"
       , "#####.##."
       , "#####.##."
       , "..##..###"
       , "#....#..#"
       ]
