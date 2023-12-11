module GridUtils (gridFromList, neighbourMapO, neighbourMapD, neighbourMap, buildNeighbours, directionsO, directionsD, directionsO3, directionsD3,
    getValue, changeValue, getNeighbours, addPoints,
    gridBounds, showCharGrid, showStringGrid, showGrid, showGrid1,
    simplePathO, simplePathO',
    Grid, NeighbourMap, Point, Point3) where
import MUtils
import qualified Data.Map as Map
import Data.List

type Point = (Int, Int)
type Point3 = (Int, Int, Int)

type Grid a = Map.Map Point a
type NeighbourMap a = Map.Map Point (a, [(Point, a)])


gridFromList :: [[a]] -> Grid a
gridFromList ass = zipWithIndexes ass |> map (\(as,y) -> zipWithIndexes as |> map (\(a,x) -> Map.singleton (x,y) a)) |> concat |> Map.unions

neighbourMapO :: Grid a -> NeighbourMap a
neighbourMapO = neighbourMap directionsO

neighbourMapD :: Grid a -> NeighbourMap a
neighbourMapD = neighbourMap directionsD

neighbourMap :: [(Int,Int)] -> Grid a -> NeighbourMap a
neighbourMap directions grid = Map.toList grid |> map (\(point, a) -> (point, buildNeighbours directions grid point) ) |> Map.fromList

buildNeighbours :: [(Int,Int)] -> Grid a -> Point -> (a, [(Point, a)])  --TODO: combine membership check with lookup?
buildNeighbours directions grid point =  directions |> map (addPoints point) |> filter (`Map.member` grid) |> zipF (grid Map.!) |> pair (grid Map.! point)

hasNeighbour :: (a->Bool) -> Point -> NeighbourMap a -> Bool
hasNeighbour p (x,y) m = m Map.! (x,y) |> snd |> map snd |> filter p |> length |> (>0)

addPoints :: Point -> Point -> Point
addPoints (x1,y1) (x2,y2) = (x1+x2, y1+y2)

getValue :: NeighbourMap a -> Point -> a
getValue ngrid point = ngrid Map.! point |> fst

changeValue :: (a->a) -> Point -> NeighbourMap a -> NeighbourMap a --TODO: this doesn't update the nieghbours
changeValue f point = Map.adjust (mapFst f) point

getNeighbours :: NeighbourMap a -> Point -> [(Point, a)]
getNeighbours ngrid point = ngrid Map.! point |> snd

directionsO :: [(Int,Int)]
directionsO = [(0,-1), (-1,0), (1,0), (0,1)]

directionsD :: [(Int,Int)]
directionsD = [(-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1)]

directionsO3 :: [(Int,Int,Int)]
directionsO3 = [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)]

directionsD3 :: [(Int,Int,Int)]
directionsD3 = [(-1,-1,-1),(-1,-1,0),(-1,-1,1),(-1,0,-1),(-1,0,0),(-1,0,1),(-1,1,-1),(-1,1,0),(-1,1,1),(0,-1,-1),(0,-1,0),(0,-1,1),(0,0,-1),(0,0,1),(0,1,-1),(0,1,0),(0,1,1),(1,-1,-1),(1,-1,0),(1,-1,1),(1,0,-1),(1,0,0),(1,0,1),(1,1,-1),(1,1,0),(1,1,1)]

gridBounds :: Grid a -> (Int,Int)
gridBounds g = (map fst g' |> map fst |> maximum, map fst g' |> map snd |> maximum) where g' = Map.toList g

showCharGrid :: Grid Char -> [String]
showCharGrid g = [0..maxY] |> map (\y-> [0..maxX] |> map (\x-> g Map.! (x,y))) where
    (maxX, maxY) = gridBounds g

showStringGrid :: Grid String -> [String]
showStringGrid g = [0..maxY] |> map (\y-> [0..maxX] |> map (\x-> g Map.! (x,y))) |> map (joinWith "|") where
    (maxX, maxY) = gridBounds g

showGrid :: Show a => Grid a -> [String]
showGrid g = Map.map show g |> showStringGrid

showGrid1 :: Show a => Grid a -> [String]
showGrid1 g = Map.map (head . show) g |> showCharGrid

--Pathfinding

--Finds a simple orthogonal path between two points, and returns every value along the path, including the starting and ending node
simplePathO  :: Grid a -> Point -> Point -> [a]
simplePathO g p1 p2 = simplePathO' p1 p2 |> map (g Map.!)

simplePathO' :: Point -> Point -> [Point]
simplePathO' p1@(x1,y1) p2@(x2,y2)
    | x1==x2 && y1==y2 = [p1]
    | x1==x2 && y1>y2  = p1 : simplePathO' (x1, y1-1) p2
    | x1==x2 && y1<y2  = p1 : simplePathO' (x1, y1+1) p2
    | x1>x2            = p1 : simplePathO' (x1-1, y1) p2
    | x1<x2            = p1 : simplePathO' (x1+1, y1) p2

---------- Dir stuff ----------

data Dir = East | West | North | South deriving (Show, Read, Eq, Ord)








