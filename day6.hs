import           MUtils

type Race = (Int, Int) --Time, record

waysToWin :: Race -> Int
waysToWin (time, dist) = [ 1 .. time ] |> map (\b -> (time - b) * b) |> count (> dist)

part1 :: [String] -> Int
part1 lines = map (splitOn ' ') lines |> map tail |> map (filter (not . null)) |> map2 read |> \[a, b] -> zip a b |> map waysToWin |> product

part2 :: [String] -> Int
part2 lines = map (splitOn ' ') lines |> map tail |> map (filter (not . null)) |> map concat |> map read |> \[a, b] -> (a, b) |> waysToWin