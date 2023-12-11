import           Data.List
import qualified Data.Map  as Map

import           GridUtils

import           MUtils

part1 :: [String] -> Int
part1 lines = map expand lines |> concat |> transpose |> map expand |> concat |> zipWithIndexes2 |> filter2 ((== '#') . fst) |> concat |> map snd |> combinationsSelf
    |> map (\((x1, y1), (x2, y2)) -> abs (x1 - x2) + abs (y1 - y2)) |> sum
  where
    expand line = if all (== '.') line then [ line, line ] else [ line ]

type Galaxies = Grid Char

--Using 'H' for cells that are horizontally stretched, 'V' for cells that are vertically stretched, and 'B' for cells that are both
bigExpand :: [String] -> Galaxies
bigExpand lines = lines |> map (\line -> if all (== '.') line then repeat 'V' |> take (length line) else line) |> transpose
    |> map (\line -> if all (`elem` ".V") line then line |> replace '.' 'H' |> replace 'V' 'B' else line) |> transpose |> gridFromList

pathLength :: Galaxies -> Point -> Point -> Integer
pathLength g (x1, y1) (x2, y2) = horizontal + vertical
  where
    horizontal = simplePathO g (x1, y1) (x2, y1) |> tail |> map (\c -> if c `elem` "BH" then 1000000 else 1) |> sum
    vertical = simplePathO g (x2, y1) (x2, y2) |> tail |> map (\c -> if c `elem` "BV" then 1000000 else 1) |> sum

part2 :: [String] -> Integer
part2 lines = Map.toList galaxies |> filter ((== '#') . snd) |> map fst |> combinationsSelf |> map (uncurry (pathLength galaxies)) |> sum
  where
    galaxies = bigExpand lines

test = [ "...#......", ".......#..", "#.........", "..........", "......#...", ".#........", ".........#", "..........", ".......#..", "#...#....." ]