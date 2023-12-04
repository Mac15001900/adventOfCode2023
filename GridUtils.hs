module GridUtils (gridFromList, neighbourMapO, neighbourMapD, neighbourMap, buildNeighbours, directionsO, directionsD, directionsO3, directionsD3, 
    getValue, changeValue, getNeighbours, addPoints,
    Grid, NeighbourMap, Point, Point3) where
import MUtils
import qualified Data.Map as Map
import Data.List

type Point = (Int, Int)
type Point3 = (Int, Int, Int)

type Grid a = Map.Map Point a
type NeighbourMap a = Map.Map Point (a, [(Point, a)])


gridFromList :: [[a]] -> Grid a
gridFromList ass = zipWithIndexes ass |> map (\(as,y) -> zipWithIndexes as |> map(\(a,x) -> Map.singleton (x,y) a)) |> concat |> Map.unions

neighbourMapO :: Grid a -> NeighbourMap a
neighbourMapO = neighbourMap directionsO

neighbourMapD :: Grid a -> NeighbourMap a
neighbourMapD = neighbourMap directionsD

neighbourMap :: [(Int,Int)] -> Grid a -> NeighbourMap a
neighbourMap directions grid = Map.toList grid |> map (\(point, a) -> (point, buildNeighbours directions grid point) ) |> Map.fromList

buildNeighbours :: [(Int,Int)] -> Grid a -> Point -> (a, [(Point, a)])  --TODO: combine membership check with lookup?
buildNeighbours directions grid point =  directions |> map (addPoints point) |> filter ((flip Map.member) grid) |> zipF (grid Map.!) |> pair (grid Map.! point)

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











