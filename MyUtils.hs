module MyUtils (
   runOnFile, runTestOnFile, runOnFileGroup,
   putList,putListToStr, split, splitOn, count, count2, countEqual, maxOn, minOn, unique, unique', uniqueOn, indexesWhere, combinations, combinations3, combinationsSelf,
   (!!?), joinWith, zipWithIndexes, differences, indexes, zipF,
   zipWithIndexes2, empty2, empty3, setElement, setElement2, setElement3, changeElement, changeElement2, changeElement3, directions2D, directions3D, groupInto2D,
   map2, map3, filter2, filter3, zip2d, zip3d, findIndex2,
   pair, pairS, mapFst, mapSnd, mapBoth, fst3, snd3, thd3, fst4, snd4, thd4, frh4, t2toList, t3toList, t4toList, t5toList, t2fromList, t3fromList, t4fromList, t5fromList,
   flattenMaybe, removeNothing,
   repeatF, repeatUntil, examine, examineStr, examineRepeat,
   factorial, (//), nck, valueBetween, mean, meanI, sign,
   aStar, (|>), readInt
    ) where

   --readInt,runOnFile,runTestOnFile,runOnFile2,runOnFileGroup,(|>),putList,putListToStr,split,count,freq,exists,singleton,(!!?),unique,unique',uniqueOn,uniqueOn',rotateMatrix,splitOn,joinWith,valueBetween, differences, tupleMap, repeatF, examine, examineStr, examineRepeat, removeNothing, indexes, zipF, zipWithIndexes, zipWithIndexes2, indexesWhere, zip2d, zip3d, map2, map3, filter2, filter3, setElement, setElement2, setElement3, changeElement, empty2, empty3, directions2D, directions3D, flattenMaybe, combinations, groupInto2D, findIndex2, aStar, tryAStar, fst3, snd3, thd3, fst4, snd4, thd4, frh4, mapFst, mapSnd, factorial, (//), nck) where
import Control.Monad
import Data.List
import Data.Maybe
import System.IO

------------------------------------ File Utils ------------------------------------

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

------------------------------------ 1D List Utils ------------------------------------

----- Printing -----
putList :: [String] -> IO()
putList xs = joinWith "\n" xs |> putStrLn

putListToStr :: Show a => [a] -> IO()
putListToStr xs = map show xs |> joinWith "\n" |> putStrLn

----- Splitting -----
split     :: (a -> Bool) -> [a] -> [[a]]
split p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a xs = splitOn' a xs []

splitOn' :: Eq a => a -> [a] -> [a]-> [[a]]
splitOn' a [] op     = [reverse op]
splitOn' a (x:xs) op = if a==x then (reverse op):(splitOn' a xs []) else splitOn' a xs (x:op)

----- Checking properties -----
count :: (a->Bool) -> [a] -> Int
count p = length . filter p

count2 :: (a->Bool) -> [[a]] -> Int
count2 p as = map (count p) as |> sum

countEqual :: Eq a => a -> [a] -> Int
countEqual x = count (==x)

countWhile :: (a->Bool) -> [a] -> Int
countWhile p [] = 0
countWhile p (x:xs)
   | p x       = 1 + countWhile p xs
   | otherwise = 0

----- Finding elements -----
maxOn :: Ord b => (a->b) -> [a] -> a
maxOn f [x] = x
maxOn f (x:xs) = if f x >= f best then x else best where best = maxOn f xs

minOn :: Ord b => (a->b) -> [a] -> a
minOn f [x] = x
minOn f (x:xs) = if f x <= f best then x else best where best = maxOn f xs

----- Filtering -----
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

indexesWhere :: (a->Bool) -> [a] -> [Int]
indexesWhere p xs = zipWithIndexes xs |> filter (p . fst) |> map snd

----- Combinations -----

combinations :: [a] -> [b] -> [(a,b)]
combinations [] _ = []
combinations (a:as) bs = map (a,) bs ++ combinations as bs

combinations3 :: [a] -> [b] -> [c] -> [(a,b,c)]
combinations3 as bs cs = map (\a-> map (\b-> map (\c-> (a,b,c)) cs) bs) as |> concat |> concat

--Produces all combinations of two elements from an array, without symmetric of reflective ones. [x,y,z] produces [(x,y),(x,z),(y,z)], but not (y,x) or (x,x)
combinationsSelf :: [a] -> [(a,a)]
combinationsSelf [] = []
combinationsSelf (x:xs) = map (x,) xs ++ combinationsSelf xs

----- Other -----

--Equivalent of !!, but returns Nothing if there is no such element in a list and Just a otherwise
(!!?) :: [a] -> Int -> Maybe a
list !!? index = if index<0 || index>=length list then Nothing else Just (list!!index)

joinWith :: [a] -> [[a]] -> [a]
joinWith a [] = []
joinWith a [x] = x
joinWith a (x:xs) = (x++a)++(joinWith a xs)

zipWithIndexes :: [a] -> [(a,Int)]
zipWithIndexes a = zip a (indexes a)

--Truns a list of numeric values into a list of differences between neighbouring values
differences :: Num a => [a] -> [a]
differences [] = []
differences a = zip (tail a) (init a) |>  map (uncurry (-))

indexes :: [a] -> [Int]
indexes [] = []
indexes a = [0..(length a)-1]

zipF :: (a->b) -> [a] -> [(a,b)]
zipF f xs = zip xs (map f xs)



------------------------------------ Higher-dimension List Utils ------------------------------------

zipWithIndexes2 :: [[a]] -> [[(a,(Int,Int))]]
zipWithIndexes2 a =  map zipWithIndexes a |> zipWithIndexes |> map (\(as, y)-> map (\(p, x)-> (p,(x,y))) as)

empty2 :: Eq a => [[a]] -> Bool
empty2 xs = not $ any (/=[]) xs

empty3 :: Eq a => [[[a]]] -> Bool
empty3 xss = map (not . any (/=[])) xss |> and

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

changeElement2 :: Int -> Int -> (a->a) -> [[a]] -> [[a]]
changeElement2 i j f xs = (take j xs)++[changeElement i f (xs!!j)]++(drop (j+1) xs)

changeElement3 :: Int -> Int -> Int -> (a->a) -> [[[a]]] -> [[[a]]]
changeElement3 i j k f xs = (take k xs)++[changeElement2 i j f (xs!!k)]++(drop (k+1) xs)

directions2D :: [(Int,Int)]
directions2D = [(-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1)]

directions3D :: [(Int,Int,Int)]
directions3D = [(-1,-1,-1),(-1,-1,0),(-1,-1,1),(-1,0,-1),(-1,0,0),(-1,0,1),(-1,1,-1),(-1,1,0),(-1,1,1),(0,-1,-1),(0,-1,0),(0,-1,1),(0,0,-1),(0,0,1),(0,1,-1),(0,1,0),(0,1,1),(1,-1,-1),(1,-1,0),(1,-1,1),(1,0,-1),(1,0,0),(1,0,1),(1,1,-1),(1,1,0),(1,1,1)]

--Groups elements of a list into groups of a specified size
groupInto2D :: Int -> [a] -> [[a]]
groupInto2D _ [] = []
groupInto2D n as = (take n as):(groupInto2D n (drop n as))

---- Higher-dimension versions of prelude functions ----
map2 :: (a->b) -> [[a]] -> [[b]]
map2 f = map (map f)

map3 :: (a->b) -> [[[a]]] -> [[[b]]]
map3 f = map (map (map f))

filter2 :: (a->Bool) -> [[a]] -> [[a]]
filter2 p = map (filter p)

filter3 :: (a->Bool) -> [[[a]]] -> [[[a]]]
filter3 p = map2 (filter p)

--zip3 exists and combines 3 lists, so using 2d and 3d here
zip2d :: [[a]] -> [[b]] -> [[(a,b)]]
zip2d [] _ = []
zip2d _ [] = []
zip2d (a:as) (b:bs) = (zip a b):(zip2d as bs)

zip3d :: [[[a]]] -> [[[b]]] -> [[[(a,b)]]]
zip3d [] _ = []
zip3d _ [] = []
zip3d (a:as) (b:bs) = (zip2d a b):(zip3d as bs)

findIndex2 :: (a->Bool) ->[[a]] -> (Int, Int)
findIndex2 p []           = error "Element not found"
findIndex2 p (xs:xss) = case findIndex p xs of
   Nothing -> mapSnd (+1) (findIndex2 p xss)
   (Just i)-> (i,0)

------------------------------------ Tuple Utils ------------------------------------

pair :: a -> b -> (a, b)
pair a b = (a,b)
pairS :: a -> b -> (b, a)
pairS a b = (b, a)

mapFst :: (a->c) -> (a,b) -> (c, b)
mapFst f (a,b) = (f a, b)
mapSnd :: (b->c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)
mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x,y) = (f x, f y)

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

t2toList :: (a,a) -> [a]
t2toList (x,y) = [x,y]
t3toList :: (a,a,a) -> [a]
t3toList (x,y,z) = [x,y,z]
t4toList :: (a,a,a,a) -> [a]
t4toList (x1,x2,x3,x4) = [x1,x2,x3,x4]
t5toList :: (a,a,a,a,a) -> [a]
t5toList (x1,x2,x3,x4,x5) = [x1,x2,x3,x4,x5]

t2fromList :: [a] -> (a,a)
t2fromList [x,y] = (x,y)
t3fromList :: [a] -> (a,a,a)
t3fromList [x,y,z] = (x,y,z)
t4fromList :: [a] -> (a,a,a,a)
t4fromList [x1,x2,x3,x4] = (x1,x2,x3,x4)
t5fromList :: [a] -> (a,a,a,a,a)
t5fromList [x1,x2,x3,x4,x5] = (x1,x2,x3,x4,x5)

------------------------------------ Maybe Utils ------------------------------------

flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe Nothing = Nothing
flattenMaybe (Just Nothing) = Nothing
flattenMaybe (Just (Just a)) = Just a

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (Nothing:xs) = removeNothing xs
removeNothing ((Just a):xs) = a:(removeNothing xs)

------------------------------------ Function Utils ------------------------------------

repeatF :: Int -> (a->a) -> a -> a
repeatF 0 _ x = x
repeatF n f x = repeatF (n-1) f (f x)

repeatUntil :: (a->Bool) -> (a->a) -> a -> a
repeatUntil p f a = if p a then a else repeatUntil p f (f a)


--For testing: shows the function's output for a list of inputs
examine :: Show a => Show b => (a->b) -> [a] -> IO()
examine f xs = map (\x-> (x, f x)) xs |> map (\(a,b)-> (show a)++(": ")++(show b)) |> joinWith "\n" |> putStrLn

--Similar to examine, for when the result of the examined function is a (potentially) multi-line string
examineStr :: Show a => (a->String) -> [a] -> IO()
examineStr f xs = map (\x-> (x, f x)) xs |> map (\(a,b)-> (show a)++(":\n")++b) |> joinWith "\n" |> putStrLn

--For testing: shows the results of a function's repeated applications
examineRepeat :: Show a => (a->a) -> a -> Int -> IO()
examineRepeat f a n = examine (\x-> repeatF x f a) [0..n]

------------------------------------ Math ------------------------------------

factorial :: (Integral a) => a -> a
factorial x = product [1..x]

(//) :: (Integral a, Integral b) => a -> b -> Float
x // y = (fromIntegral x) / (fromIntegral y)

nck :: (Integral a) => a -> a -> a
nck n k = (factorial n) `div` ((factorial k) * (factorial (n-k)))

valueBetween :: Ord a => (a,a) -> a -> Bool
valueBetween (low,high) x = x >= low && x <= high

mean :: Fractional a => [a] -> a
mean as = (sum as) / (length as |> fromIntegral)

meanI ::  Integral a => [a] -> Float
meanI as = (sum as |> fromIntegral) / (length as |> fromIntegral)

sign :: Int -> Int
sign x
      | x > 0  = 1
      | x == 0 = 0
      | x < 0  = -1

------------------------------------ A* ------------------------------------

--               Neighbours         Heuristic  Start  isTarget    Cost of shortest path
aStar :: Eq a => (a->[(a, Int)]) -> (a->Int) -> a -> (a->Bool) -> Int
aStar neighbours heuristic start target = fromMaybe (error "aStar: No path found") res
   where res = aStar' neighbours heuristic [(start, 0, heuristic start)] [] target

tryAStar :: Eq a => (a->[(a, Int)]) -> (a->Int) -> a -> (a->Bool) -> Maybe Int
tryAStar neighbours heuristic start target = aStar' neighbours heuristic [(start, 0, heuristic start)] [] target

--                neighbours        heuristic    frontier          visited  isTarget
aStar' :: Eq a => (a->[(a, Int)]) -> (a->Int) -> [(a, Int, Int)] ->  [a] -> (a->Bool) -> Maybe Int
aStar' _  _ []              _  _ = Nothing --The frontier being empty means we've explored everything
aStar' ns h (next:frontier) vs t
   | t (fst3 next)         = Just (snd3 next)
   | (fst3 next) `elem` vs = aStar' ns h frontier vs t
   | otherwise             = aStar' ns h (expandFrontier frontier newNodes) ((fst3 next):vs) t where
      newNodes = ns (fst3 next) |> filter (\(a,_)-> a `notElem` vs) |> map (\(a,c) -> (a, c + snd3 next, h a))

expandFrontier :: [(a, Int, Int)] -> [(a, Int, Int)] -> [(a, Int, Int)]
expandFrontier frontier newNodes = foldl insertNode frontier newNodes

insertNode :: [(a, Int, Int)] -> (a, Int, Int) -> [(a, Int, Int)]
insertNode [] node = [node]
insertNode ((a1,c1,h1):xs) (a2,c2,h2) = if c2+h2 < c1+h1 then (a2,c2,h2):(a1,c1,h1):xs else (a1,c1,h1):(insertNode xs (a2,c2,h2))


------------------------------------ Misc ------------------------------------

(|>) :: a -> (a->b) -> b
a |> f = f a

readInt :: String -> Int
readInt = read



-- Notes about updating from MyUtils:
-- Replace exists with any
-- Replace rotateMatrix with transpose
-- Replace "tupleMap f" with "map (uncurry f)"
-- Replace "freq xs x" with "countEqual x xs"
-- Replace "separate" with "swap . break"