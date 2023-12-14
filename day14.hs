import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe ( fromJust )
import qualified Data.Set   as Set

import           GridUtils

import           MUtils

type Cubes = Grid Bool

type Rocks = Grid Bool

type Dish = (Cubes, Rocks)

parse :: [String] -> Dish
parse lines = (map2 (== '#') lines |> gridFromList, map2 (== 'O') lines |> gridFromList)

moveAll :: Cubes -> Point -> Rocks -> Rocks
moveAll c dir rocks = if rocks == newRocks then rocks else moveAll c dir newRocks
  where
    newRocks = move c dir rocks

move :: Cubes -> Point -> Rocks -> Rocks
move c dir rocks = Map.toList rocks |> filter snd |> map fst |> foldr (moveRock c dir) rocks

moveRock :: Cubes -> Point -> Point -> Rocks -> Rocks
moveRock cubes dir rock rocks
    | not $ rocks Map.! rock = rocks
    | Map.notMember newPoint rocks = rocks
    | cubes Map.! newPoint = rocks
    | rocks Map.! newPoint = moveRock cubes dir newPoint rocks -- |> Map.insert rock False |> Map.insert newPoint True
    | otherwise = rocks |> Map.insert rock False |> Map.insert newPoint True |> moveRock cubes dir newPoint
  where
    newPoint = addPoints dir rock

part1 :: [String] -> Int
part1 lines = moveAll cubes (0, -1) rocks |> Map.toList |> filter snd |> map fst |> map snd |> map (height -) |> sum
  where
    height = length lines
    (cubes, rocks) = parse lines

partTest :: [String] -> [String]
partTest lines = moveAll cubes (0, -1) rocks |> showBoard cubes
  where
    height = length lines
    (cubes, rocks) = parse lines

type History = Map.Map Rocks Integer

type Period = (Integer, Integer) --Start of period, length of period

runCycle :: Cubes -> Rocks -> Rocks
runCycle cubes rocks = foldl (flip (moveAll cubes)) rocks [ (0, -1), (-1, 0), (0, 1), (1, 0) ]

moveALot :: Cubes -> Rocks -> Integer -> History -> Integer -> Rocks
moveALot cubes rocks target history current
    | Map.member rocks history = Map.toList history |> find ((== index) . snd) |> fromJust |> fst
  where
    pStart = history Map.! rocks
    pLength = current - pStart
    index = ((target - pStart) `mod` pLength) + pStart
moveALot cubes rocks target history current = moveALot cubes (runCycle cubes rocks) target (Map.insert rocks current history) (current + 1)

part2 :: [String] -> Int
part2 lines = moveALot cubes rocks 1000000000 Map.empty 0 |> Map.toList |> filter snd |> map fst |> map snd |> map (height -) |> sum
  where
    height = length lines
    (cubes, rocks) = parse lines

showBoard :: Cubes -> Rocks -> [String]
showBoard c r = zip2d (Map.map (\isCube -> if isCube then '#' else '.') c |> showCharGrid) (Map.map (\isRock -> if isRock then 'O' else '.') r |> showCharGrid)
    |> map2 (\(c, r) -> if c == '#' then '#' else r)

    --zip2d (showGrid c) (showGrid r) |> map (splitOn '|') |> map2 (mapBoth read) |> map2 (\(isCube, isRock)-> if isCube then '#' else if isRock then 'O' else '.')
test = [ "O....#....", "O.OO#....#", ".....##...", "OO.#O....O", ".O.....O#.", "O.#..O.#.#", "..O..#O..O", ".......O..", "#....###..", "#OO..#...." ]