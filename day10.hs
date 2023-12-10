import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe
import qualified Data.Set   as Set

import           GridUtils

import           MUtils

import           Prelude    hiding ( Left, Right )

type Pipes = Grid Char

data Dir = Left | Right | Up | Down
    deriving ( Show, Read, Eq, Ord )

pipeDirs :: Char -> [Dir]
pipeDirs c = Map.fromList [ ('|', [ Up, Down ])
                          , ('-', [ Left, Right ])
                          , ('L', [ Up, Right ])
                          , ('J', [ Up, Left ])
                          , ('7', [ Down, Left ])
                          , ('F', [ Down, Right ])
                          , ('V', [ Up, Down ])
                          , ('H', [ Left, Right ])
                          ] Map.! c

dirToPoint :: Dir -> Point
dirToPoint d = Map.fromList [ (Left, (-1, 0)), (Right, (1, 0)), (Up, (0, -1)), (Down, (0, 1)) ] Map.! d

reverseDir :: Dir -> Dir
reverseDir d = Map.fromList [ (Left, Right), (Right, Left), (Up, Down), (Down, Up) ] Map.! d

measurePath :: Pipes -> Point -> Dir -> Int
measurePath ps pos dir
    | pipe == 'S' = 0
    | otherwise = 1 + measurePath ps (addPoints pos (dirToPoint newDir)) newDir
  where
    pipe = ps Map.! pos
    newDir = pipeDirs pipe |> filter (/= reverseDir dir) |> head

part1 :: [String] -> Int
part1 lines = (measurePath pipes (addPoints startPos (dirToPoint dir)) dir + 1) `div` 2
  where
    pipes = gridFromList lines
    startPos = findIndex2 (== 'S') lines
    dir = [ Left, Right, Up, Down ] |> filter (\d -> pipes Map.!? addPoints startPos (dirToPoint d) |> maybe [] pipeDirs |> (reverseDir d `elem`)) |> head

expandTile :: Char -> [[Char]]
expandTile '.' = [ "___", "_._", "___" ]
expandTile '-' = [ "___", "-H-", "___" ] --'H' marks the centre of a horizontal pipe; it gets counted for area, while '-' is not
expandTile '|' = [ "_|_", "_V_", "_|_" ] --Same with 'V' instead of '|'
expandTile 'L' = [ "_|_", "_L-", "___" ]
expandTile 'J' = [ "_|_", "-J_", "___" ]
expandTile '7' = [ "___", "-7_", "_|_" ]
expandTile 'F' = [ "___", "_F-", "_|_" ]
expandTile 'S' = [ "_|_", "-S-", "_|_" ] --As long as only S gets those extra expansions they won't block anything

buildLargePipes :: [String] -> Pipes
buildLargePipes lines = map2 expandTile lines |> map transpose |> concat |> map concat |> gridFromList

findMainPath :: Pipes -> Point -> Dir -> [Point]
findMainPath ps pos dir
    | pipe == 'S' = [ pos ]
    | otherwise = pos : findMainPath ps (addPoints pos (dirToPoint newDir)) newDir
  where
    pipe = ps Map.!? pos |> fromJust
    newDir = pipeDirs pipe |> filter (/= reverseDir dir) |> head

countArea :: Pipes -> Set.Set Point -> [Point] -> [Point] -> Int
countArea ps mainPath [] visited = 0
countArea ps mainPath (x : xs) visited
    | Map.notMember x ps = error $ (show x) ++ " not in map"
countArea ps mainPath (x : xs) visited = (if ps Map.! x `elem` ".HVLJ7F" then 1 else 0) + countArea ps mainPath frontier' (x : visited)
  where
    frontier' = [ Left, Right, Up, Down ] |> map dirToPoint |> map (addPoints x) |> filter (`Map.member` ps) |> filter (`notElem` visited)
        |> filter (`notElem` xs) |> filter (`Set.notMember` mainPath) |> (++ xs)

part2 :: [String] -> Int
part2 lines = countArea pipes (findMainPath pipes (addPoints startPos (dirToPoint (head dirs))) (head dirs) |> Set.fromList) [ areaStart ] []
  where
    pipes = buildLargePipes lines
    startPos = Map.toList pipes |> find ((== 'S') . snd) |> fromJust |> fst
    dirs = [ Left, Right, Up, Down ] |> filter (\d -> pipes Map.!? addPoints startPos (dirToPoint d |> mapBoth (* 2)) |> maybe False (`notElem` "_."))
    areaStart = map dirToPoint dirs |> foldl1 addPoints |> addPoints startPos

test = [ "-L|F7", "7S-7|", "L|7||", "-L-J|", "L|-JF" ]

test2 = [ "..F7.", ".FJ|.", "SJ.L7", "|F--J", "LJ..." ]

test3 = [ "FF7FSF7F7F7F7F7F---7"
        , "L|LJ||||||||||||F--J"
        , "FL-7LJLJ||||||LJL-77"
        , "F--JF--7||LJLJ7F7FJ-"
        , "L---JF-JLJ.||-FJLJJ7"
        , "|F|F-JF---7F7-L7L|7|"
        , "|FFJF7L7F-JF7|JL---7"
        , "7-L-JL7||F7|L7F-7F7|"
        , "L.L7LFJ|||||FJL7||LJ"
        , "L7JLJL-JLJLJL--JLJ.L"
        ]