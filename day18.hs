{-# LANGUAGE TupleSections #-}

import qualified Data.Map  as Map
import qualified Data.Set  as Set

import           GridUtils

import           MUtils

type Trench = Set.Set Point

type Instruction = (Dir, Int)

digTrench :: Point -> Trench -> [Instruction] -> Trench
digTrench _ t [] = t
digTrench p t ((d, 0) : is) = digTrench p t is
digTrench p t ((d, l) : is) = digTrench (stepDir d p) (Set.insert p t) ((d, l - 1) : is)

parseInstruction :: String -> Instruction
parseInstruction line = splitOn ' ' line |> take 2 |> t2fromList |> mapSnd read |> mapFst (Map.fromList [ ("R", East), ("L", West), ("U", North), ("D", South) ] Map.!)

--https://xkcd.com/2021/
countArea :: Point -> Trench -> Int
countArea start trench = bfsAllCosts (\p -> directionsO |> map (addPoints p) |> filter (`Set.notMember` trench) |> map (, 1)) start |> Map.size

part1 :: [String] -> Int
part1 lines = countArea (0, -1) trench + Set.size trench
  where
    trench = map parseInstruction lines |> digTrench (0, 0) Set.empty

test = [ "R 6 (#70c710)"
       , "D 5 (#0dc571)"
       , "L 2 (#5713f0)"
       , "D 2 (#d2c081)"
       , "R 2 (#59c680)"
       , "D 2 (#411b91)"
       , "L 5 (#8ceee2)"
       , "U 2 (#caa173)"
       , "L 1 (#1b58a2)"
       , "U 2 (#caa171)"
       , "R 2 (#7807d2)"
       , "U 3 (#a77fa3)"
       , "L 2 (#015232)"
       , "U 2 (#7a21e3)"
       ]