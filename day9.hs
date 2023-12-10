import           MUtils

predict :: Bool -> [Integer] -> Integer
predict backwards xs
    | all (== 0) xs = 0
    | backwards = head xs - predict backwards (differences xs)
    | otherwise = last xs + predict backwards (differences xs)

part1 :: [String] -> Integer
part1 lines = map (splitOn ' ') lines |> map2 read |> map (predict False) |> sum

part2 :: [String] -> Integer
part2 lines = map (splitOn ' ') lines |> map2 read |> map (predict True) |> sum

test = [ "0 3 6 9 12 15", "1 3 6 10 15 21", "10 13 16 21 30 45" ]