import MUtils

type Cubes = [Integer]
type Game = (Integer, [Cubes])

parseGame :: String -> Game
parseGame line = (splitOn ':' line |> head |> splitOn ' ' |> last |> read, 
    splitOn ':' line |> last |> filter (/= ',') |> splitOn ';' |> map (splitOn ' ') |> map tail |> map parseCubes)

parseCubes :: [String] -> Cubes
parseCubes [] = [0,0,0]
parseCubes (n:"red"  :xs) = zipWith (+) [read n, 0, 0] (parseCubes xs)
parseCubes (n:"green":xs) = zipWith (+) [0, read n, 0] (parseCubes xs)
parseCubes (n:"blue" :xs) = zipWith (+) [0, 0, read n] (parseCubes xs)

part1 :: [String] -> Integer
part1 lines = map parseGame lines |> filter (\(_,c)-> map (zipWith (>=) [12,13,14]) c |> map and |> and) |> map fst |> sum

part2 :: [String] -> Integer
part2 lines = map parseGame lines |> map snd |> map (foldl (zipWith max) [0,0,0]) |> map (foldl (*) 1) |> sum


test= ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]