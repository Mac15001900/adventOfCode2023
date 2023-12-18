{-# LANGUAGE TupleSections #-}

import           Data.List
import qualified Data.Map  as Map

import           GridUtils
import           MUtils

type State = (Point, Dir, Int) --Current position, facing direction, how many more times can we move straight
type City = Grid Int

nextStates :: City -> State -> [(State, Int)]
nextStates city (point, dir, blocks) = (if blocks == 0 then [ left, right ] else [ straight, left, right ]) |> filter (\(p, _, _) -> Map.member p city)
    |> map (\(p, d, b) -> ((p, d, b), city Map.! p))
  where
    straight = (stepDir dir point, dir, blocks - 1)
    left = (stepDir (rotateDirA dir) point, rotateDirA dir, 2)
    right = (stepDir (rotateDirC dir) point, rotateDirC dir, 2)

part1 :: [String] -> Int
part1 lines = aStar (nextStates (map2 singleton lines |> map2 read |> gridFromList)) (pointDistanceO endPoint . fst3) ((0, 0), East, 3) ((== endPoint) . fst3)
  where
    endPoint = (length (head lines) - 1, length lines - 1)

nextStatesUltra :: City -> State -> [(State, Int)]
nextStatesUltra city (point, dir, blocks) = (if blocks > 6 then [ straight ] else if blocks == 0 then [ left, right ] else [ straight, left, right ])
    |> filter (\(p, _, _) -> Map.member p city) |> map (\(p, d, b) -> ((p, d, b), city Map.! p))
  where
    straight = (stepDir dir point, dir, blocks - 1)
    left = (stepDir (rotateDirA dir) point, rotateDirA dir, 9)
    right = (stepDir (rotateDirC dir) point, rotateDirC dir, 9)

part2 :: [String] -> Int
part2 lines = map (\start -> aStar (nextStatesUltra city) ((hMap Map.!) . fst3) start (\(p, _, b) -> p == endPoint && b <= 6)) [ ((0, 0), East, 10), ((0, 0), South, 10) ]
    |> minimum
  where
    city = map2 singleton lines |> map2 read |> gridFromList
    endPoint = (length (head lines) - 1, length lines - 1)
    hMap = bfsAllCosts (\p -> directionsO |> map (addPoints p) |> filter (`Map.member` city) |> map (, city Map.! p)) endPoint

test = [ "2413432311323"
       , "3215453535623"
       , "3255245654254"
       , "3446585845452"
       , "4546657867536"
       , "1438598798454"
       , "4457876987766"
       , "3637877979653"
       , "4654967986887"
       , "4564679986453"
       , "1224686865563"
       , "2546548887735"
       , "4322674655533"
       ]

test2 = [ "111111111111", "999999999991", "999999999991", "999999999991", "999999999991" ]

test3 = [ "111111111111", "999999999991", "999999999991", "999999999991", "999999999991" ]

main :: IO ()
main = runOnFile "input17.txt" part2