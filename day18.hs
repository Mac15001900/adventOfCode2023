{-# LANGUAGE TupleSections #-}

import           Data.Char ( digitToInt )
import           Numeric   ( readHex )
import qualified Data.Map  as Map
import           GridUtils hiding ( Point )
import           MUtils

type Point = (Integer, Integer)
type Instruction = (Dir, Integer)

parseInstruction :: String -> Instruction
parseInstruction line = splitOn ' ' line |> take 2 |> t2fromList |> mapSnd read |> mapFst (Map.fromList [ ("R", East), ("L", West), ("U", North), ("D", South) ] Map.!)

parseInstruction2 :: String -> Instruction
parseInstruction2 line = splitOn ' ' line |> (!! 2) |> filter (`notElem` "()#") |> \n -> (last n, init n) |> mapFst digitToInt
    |> mapFst ([ East, South, West, North ] !!) |> mapSnd (fst . head . readHex)

buildPath :: [Point] -> [Instruction] -> [Point]
buildPath path [] = path
buildPath (p : ps) ((dir, n) : is) = buildPath (moveDir dir n p : p : ps) is

--Using shoelace fomula for the area - this underestimates each edge point by 0.5, so we add half the path length to compensate.
--It also underestimates each corner by 0.25 or 0.75 - since there's 4 more turns towards the inside, to compensate we add 1 at the end
countArea :: [Point] -> Integer
countArea points = zip points (tail points ++ [ head points ]) |> map (\((x1, y1), (x2, y2)) -> x1 * y2 - x2 * y1) |> sum |> abs
    |> (+ lineLength points) |> (`div` 2) |> (+ 1)

lineLength :: [Point] -> Integer
lineLength [p] = 0
lineLength (p1 : p2 : ps) = pointDistanceO p1 p2 + lineLength (p2 : ps)

part1 :: [String] -> Integer
part1 lines = map parseInstruction lines |> buildPath [ (0, 0) ] |> countArea

part2 :: [String] -> Integer
part2 lines = map parseInstruction2 lines |> buildPath [ (0, 0) ] |> countArea

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