

module MyUtils (readInt,runOnFile,runTestOnFile,runOnFile2,runOnFileGroup,(|>),putList,putListToStr,split,count,freq,exists,singleton,(!!?),unique,unique',uniqueOn,uniqueOn',rotateMatrix,splitOn,joinWith,valueBetween, differences, tupleMap, repeatF, examine, examineStr, examineRepeat, removeNothing, indexes, zipWithIndexes, zipWithIndexes2, indexesWhere, zip2d, zip3d, map2, map3, filter2, filter3, setElement, setElement2, setElement3, changeElement, empty2, empty3, directions2D, directions3D, flattenMaybe, combinations, groupInto2D, findIndex2, aStar, tryAStar, fst3, snd3, thd3, fst4, snd4, thd4, frh4, mapFst, mapSnd, factorial, (//), nck) where
import Control.Monad
import Data.List
import Data.Maybe
import System.IO

(|>) :: a -> (a->b) -> b
a |> f = f a

readInt :: String -> Int
readInt = read

--Takes a file path and a function, runs that function on the file's contents, and prints the function's output. Trims the last line of the file if it's an empty line
runOnFile :: Show a => String -> ([String]->a) -> IO ()
runOnFile input start = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = splitOn '\n' contents
   let linesTrimmed = if last lines == "" then init lines else lines
   print $ start linesTrimmed
   hClose handle

runTestOnFile :: String -> ([String]->IO()) -> IO ()
runTestOnFile input start = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = splitOn '\n' contents
   let linesTrimmed = if last lines == "" then init lines else lines
   start linesTrimmed
   hClose handle

--Same as run on file, but splits the resulting array of strings by empty lines
runOnFileGroup :: Show a => String -> ([[String]]->a) -> IO ()
runOnFileGroup input start = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = splitOn '\n' contents
   let linesTrimmed = if last lines == "" then init lines else lines
   let res = splitOn "" linesTrimmed
   print $ start res
   hClose handle   
   
runOnFile2 :: ([String]->String) -> String -> IO ()
runOnFile2 start input = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = split (=='\n') contents
   putStrLn $ start lines
   hClose handle

putList :: [String] -> IO()
putList xs = joinWith "\n" xs |> putStrLn

putListToStr :: Show a => [a] -> IO()
putListToStr xs = map show xs |> joinWith "\n" |> putStrLn
   
split     :: (a -> Bool) -> [a] -> [[a]]
split p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

count :: (a->Bool) -> [a] -> Int
count p = length . (filter p)

freq :: Eq a => [a] -> a -> Int
freq [] _     = 0
freq (x:xs) a = (if x==a then 1 else 0) + (freq xs a)

exists :: (a->Bool) -> [a] -> Bool
exists p xs = isJust (find p xs) 

--Creates a singleton for an element, equivalent to base 4.15
singleton :: a -> [a]
singleton x = [x]

--Equivalent of !!, but returns Nothing if there is no such element in an array and Just a otherwise
(!!?) :: [a] -> Int -> Maybe a
list !!? index = if index<0 || index>=length list then Nothing else Just (list!!index)

--Removes duplicates from a list
unique :: Eq a  => [a] -> [a]
unique xs = xs |> reverse |> unique' |> reverse

--Removes duplicates from a list faster, but messes up the order
unique' :: Eq a => [a] -> [a]
unique' []     = []
unique' (x:xs) = if x `elem` xs then unique' xs else x:unique' xs

uniqueOn :: Eq b => (a->b) -> [a] -> [a]
uniqueOn f as = as |> reverse |> uniqueOn' f |> reverse

uniqueOn' :: Eq b => (a->b) -> [a] -> [a]
uniqueOn' f as = uniqueOn'' as (map f as)

uniqueOn'' :: Eq b => [a] -> [b] -> [a]
uniqueOn'' [] _ = []
uniqueOn'' (a:as) (b:bs) = if b `elem` bs then uniqueOn'' as bs else a:uniqueOn'' as bs

rotateMatrix :: [[a]] -> [[a]]
rotateMatrix (x:xs) = foldr largerZip (map (\a->[a]) x) (reverse xs) |> map reverse

largerZip :: [a] -> [[a]] -> [[a]]
largerZip []     []       = []
largerZip (x:xs) (ys:yss) = (x:ys):(largerZip xs yss)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a xs = splitOn' a xs []

splitOn' :: Eq a => a -> [a] -> [a]-> [[a]]
splitOn' a [] op     = [reverse op]
splitOn' a (x:xs) op = if a==x then (reverse op):(splitOn' a xs []) else splitOn' a xs (x:op)

joinWith :: [a] -> [[a]] -> [a]
joinWith a [] = []
joinWith a [x] = x
joinWith a (x:xs) = (x++a)++(joinWith a xs)

valueBetween :: Ord a => (a,a) -> a -> Bool
valueBetween (low,high) x = x >= low && x <= high

differences :: Num a => [a] -> [a]
differences [] = []
differences a = zip (tail a) (init a) |>  tupleMap (-)

tupleMap :: (a->b->c) -> [(a,b)] -> [c]
tupleMap f = map (\(a,b) -> f a b)

repeatF :: Int -> (a->a) -> a -> a
repeatF 0 _ x = x
repeatF n f x = repeatF (n-1) f (f x)

--For testing: shows the function's output for a list of inputs
examine :: Show a => Show b => (a->b) -> [a] -> IO()
examine f xs = map (\x-> (x, f x)) xs |> map (\(a,b)-> (show a)++(": ")++(show b)) |> joinWith "\n" |> putStrLn

--Similar to examine, for when the result of the examined function is a (potentially) multi-line string
examineStr :: Show a => (a->String) -> [a] -> IO()
examineStr f xs = map (\x-> (x, f x)) xs |> map (\(a,b)-> (show a)++(":\n")++b) |> joinWith "\n" |> putStrLn

--For testing: shows the results of a function's repeated applications
examineRepeat :: Show a => (a->a) -> a -> Int -> IO()
examineRepeat f a n = examine (\x-> (repeatF x f a)) [0..n]

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (Nothing:xs) = removeNothing xs
removeNothing ((Just a):xs) = a:(removeNothing xs)

indexes :: [a] -> [Int]
indexes [] = []
indexes a = [0..(length a)-1]

zipWithIndexes :: [a] -> [(a,Int)]
zipWithIndexes a = zip a (indexes a)

zipWithIndexes2 :: [[a]] -> [[(a,(Int,Int))]]
zipWithIndexes2 a =  map zipWithIndexes a |> zipWithIndexes |> map (\(as, y)-> map (\(p, x)-> (p,(x,y))) as)

indexesWhere :: (a->Bool) -> [a] -> [Int]
indexesWhere p xs = zipWithIndexes xs |> filter (p . fst) |> map snd

zip2d :: [[a]] -> [[b]] -> [[(a,b)]]
zip2d [] _ = []
zip2d _ [] = []
zip2d (a:as) (b:bs) = (zip a b):(zip2d as bs)

zip3d :: [[[a]]] -> [[[b]]] -> [[[(a,b)]]]
zip3d [] _ = []
zip3d _ [] = []
zip3d (a:as) (b:bs) = (zip2d a b):(zip3d as bs)


map2 :: (a->b) -> [[a]] -> [[b]]
map2 f = map (map f)

map3 :: (a->b) -> [[[a]]] -> [[[b]]]
map3 f = map (map (map f))

filter2 :: (a->Bool) -> [[a]] -> [[a]]
filter2 p = map (filter p)

filter3 :: (a->Bool) -> [[[a]]] -> [[[a]]]
filter3 p = map2 (filter p)

empty2 :: Eq a => [[a]] -> Bool
empty2 xs = not $ exists (/=[]) xs

empty3 :: Eq a => [[[a]]] -> Bool
empty3 xss = (map (\xs->not $ exists (/=[]) xs) xss |> and)

setElement :: Int -> a -> [a] -> [a]
setElement i x xs = (take i xs)++[x]++(drop (i+1) xs)

setElement2 :: Int -> Int -> a -> [[a]] -> [[a]]
setElement2 i j x xs = (take j xs)++[setElement i x (xs!!j)]++(drop (j+1) xs)

setElement3 :: Int -> Int -> Int -> a -> [[[a]]] -> [[[a]]]
setElement3 i j k x xs = (take k xs)++[setElement2 i j x (xs!!k)]++(drop (k+1) xs)

changeElement :: Int -> (a->a) -> [a] -> [a]
changeElement i f [] = []
changeElement 0 f (x:xs) = (f x):xs
changeElement i f (x:xs) = x:(changeElement (i-1) f xs)

directions2D :: [(Int,Int)]
directions2D = [(-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1)]

directions3D :: [(Int,Int,Int)]
directions3D = [(-1,-1,-1),(-1,-1,0),(-1,-1,1),(-1,0,-1),(-1,0,0),(-1,0,1),(-1,1,-1),(-1,1,0),(-1,1,1),(0,-1,-1),(0,-1,0),(0,-1,1),(0,0,-1),(0,0,1),(0,1,-1),(0,1,0),(0,1,1),(1,-1,-1),(1,-1,0),(1,-1,1),(1,0,-1),(1,0,0),(1,0,1),(1,1,-1),(1,1,0),(1,1,1)]

flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe Nothing = Nothing
flattenMaybe (Just Nothing) = Nothing
flattenMaybe (Just (Just a)) = Just a

combinations :: [a] -> [b] -> [(a,b)]
combinations [] _ = []
combinations (a:as) bs = (map (\b-> (a,b)) bs)++(combinations as bs)

--Groups elements of a list into groups of a specified size
groupInto2D :: Int -> [a] -> [[a]]
groupInto2D _ [] = []
groupInto2D n as = (take n as):(groupInto2D n (drop n as))

{-
findIndex :: (a->Bool) -> [a] -> Int
findIndex p []     = error "Element not found"
findIndex p (x:xs) = if p x then 0 else 1+(findIndex p xs)
-}

findIndex2 :: (a->Bool) ->[[a]] -> (Int, Int)
findIndex2 p []           = error "Element not found"
findIndex2 p (xs:xss) = case findIndex p xs of
   Nothing -> mapSnd (+1) (findIndex2 p xss)
   (Just i)-> (i,0)


--findIndex2 p ([]:xss)      = mapSnd (+1) (findIndex2 p xss)
--findIndex2 p ((x:xs):xss) = if p x then (0,0) else mapFst (+1) (findIndex2 p (xs:xss))

--------------------------- A*

--               Neighbours         Heuristic  Start  isTarget    Cost of shortest path
aStar :: Eq a => (a->[(a, Int)]) -> (a->Int) -> a -> (a->Bool) -> Int
aStar neighbours heuristic start target = if isJust res then fromJust res else error "aStar: No path found" 
   where res = aStar' neighbours heuristic [(start, 0, heuristic start)] [] target

tryAStar :: Eq a => (a->[(a, Int)]) -> (a->Int) -> a -> (a->Bool) -> Maybe Int
tryAStar neighbours heuristic start target = aStar' neighbours heuristic [(start, 0, heuristic start)] [] target

--                neighbours        heuristic    frontier          visited  isTarget
aStar' :: Eq a => (a->[(a, Int)]) -> (a->Int) -> [(a, Int, Int)] ->  [a] -> (a->Bool) -> Maybe Int
aStar' _  _ []              _  _ = Nothing --The frontier being empty means we've explored everything
aStar' ns h (next:frontier) vs t | t (fst3 next)         = Just (snd3 next)
                                 | (fst3 next) `elem` vs = aStar' ns h frontier vs t
                                 | otherwise             = aStar' ns h (expandFrontier frontier newNodes) ((fst3 next):vs) t where
                                      newNodes = ns (fst3 next) |> filter (\(a,_)-> not (a `elem` vs)) |> map (\(a,c) -> (a, c + snd3 next, h a)) 

expandFrontier :: [(a, Int, Int)] -> [(a, Int, Int)] -> [(a, Int, Int)]
expandFrontier frontier newNodes = foldl insertNode frontier newNodes

insertNode :: [(a, Int, Int)] -> (a, Int, Int) -> [(a, Int, Int)]
insertNode [] node = [node]
insertNode ((a1,c1,h1):xs) (a2,c2,h2) = if c2+h2 < c1+h1 then (a2,c2,h2):(a1,c1,h1):xs else (a1,c1,h1):(insertNode xs (a2,c2,h2))

---------------------------

mapFst :: (a->c) -> (a,b) -> (c, b) 
mapFst f (a,b) = (f a, b)

mapSnd :: (b->c) -> (a,b) -> (a,c) 
mapSnd f (a,b) = (a, f b)

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c

fst4 :: (a,b,c,d) -> a
fst4 (a,b,c,d) = a

snd4 :: (a,b,c,d) -> b
snd4 (a,b,c,d) = b

thd4 :: (a,b,c,d) -> c
thd4 (a,b,c,d) = c

frh4 :: (a,b,c,d) -> d
frh4 (a,b,c,d) = d

--Math

factorial :: (Integral a) => a -> a
factorial x = product [1..x]

(//) :: (Integral a, Integral b) => a -> b -> Float
x // y = (fromIntegral x) / (fromIntegral y)

nck :: (Integral a) => a -> a -> a
nck n k = (factorial n) `div` ((factorial k) * (factorial (n-k)))