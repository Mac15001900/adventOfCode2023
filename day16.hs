{-# LANGUAGE TupleSections #-}

import           Data.List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

import           GridUtils
import           MUtils

type Beam = (Point, Dir)
type Contraption = Grid Char
type History = Set.Set Beam --All the past beams. This help us stop if we've already been somewhere
type State = ([Beam], History)

simulate :: Contraption -> State -> State
simulate _ ([], h) = ([], h)
simulate c (beams, h) = foldl (\(accBeams, newHis) nextBeam -> moveBeam c newHis nextBeam |> mapFst (++ accBeams)) ([], h) (filter (`Set.notMember` h) beams) |> simulate c

--Move a beam by one step. Returns: a new history with the starting beam added, and the new beam(s)
moveBeam :: Contraption -> History -> Beam -> ([Beam], History)
moveBeam con his beam@(p, dir) = (rotateBeam (con Map.! p) beam |> map (\newDir -> (stepDir newDir p, newDir)) |> filter (\(p, _) -> Map.member p con), Set.insert beam his)

rotateBeam :: Char -> Beam -> [Dir]
rotateBeam '.' (p, dir) = [ dir ]
rotateBeam '/' (p, dir) = removeNothing [ deflectDir (East, North) dir, deflectDir (West, South) dir ]
rotateBeam '\\' (p, dir) = removeNothing [ deflectDir (East, South) dir, deflectDir (West, North) dir ]
rotateBeam '-' (p, dir)
    | isHorizontal dir = [ dir ]
    | otherwise = [ East, West ]
rotateBeam '|' (p, dir)
    | isVertical dir = [ dir ]
    | otherwise = [ North, South ]

part1 :: [String] -> Int
part1 lines = simulate (gridFromList lines) ([ ((0, 0), East) ], Set.empty) |> snd |> Set.map fst |> Set.size

beamSet :: [String] -> [Beam]
beamSet lines = (map (0, ) [ 0 .. h ] |> map (, East)) ++ (map (w, ) [ 0 .. h ] |> map (, West)) ++ (map (, 0) [ 0 .. w ] |> map (, South))
    ++ (map (, h) [ 0 .. w ] |> map (, North))
  where
    w = length (head lines) - 1
    h = length lines - 1

part2 :: [String] -> Int
part2 lines = beamSet lines |> map singleton |> map (, Set.empty) |> map (simulate (gridFromList lines)) |> map (\res -> res |> snd |> Set.map fst |> Set.size) |> maximum

test = [ ".|...\\....", "|.-.\\.....", ".....|-...", "........|.", "..........", ".........\\", "..../.\\\\..", ".-.-/..|..", ".|....-|.\\", "..//.|...." ]