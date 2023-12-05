import           MUtils

type MapPart = (Integer, Integer, Integer) -- Destination start, source start, range

parseConverters :: [String] -> [[MapPart]]
parseConverters lines = splitOn "" lines |> map tail |> map2 (splitOn ' ') |> map3 read |> map2 t3fromList

convert :: [MapPart] -> Integer -> Integer
convert [] n = n
convert ((d, s, r) : xs) n
    | valueBetween (s, s + r - 1) n = d + (n - s)
    | otherwise = convert xs n

part1 :: [String] -> Integer
part1 lines = head lines |> splitOn ' ' |> tail |> map read |> map (\seed -> foldl (flip convert) seed (drop 2 lines |> parseConverters)) |> minimum

type Range = (Integer, Integer) --Range start, range end

convert2 :: [MapPart] -> Range -> [Range]
convert2 [] rn = [ rn ]
convert2 ((d, s, r) : xs) (rs, re)
    | rs >= s && re <= s + r - 1 = [ (rs + d - s, re + d - s) ] --Range is entirely within map
    | rs >= s && rs <= s + r - 1 = (rs + d - s, d + r - 1) : convert2 xs (s + r, re) --Start in within the map, but the end isn't
    | re <= s + r - 1 && re >= s = (d, re + d - s) : convert2 xs (rs, s - 1) --End in within the map, but start isn't
    | rs < s && re > s + r - 1 = (d, d + r - 1) : convert2 xs (rs, s - 1) ++ convert2 xs (s + r, re) --The map is entirely within the range
    | otherwise = convert2 xs (rs, re) --There is no overlap at all

part2 :: [String] -> Integer
part2 lines = head lines |> splitOn ' ' |> tail |> map read |> groupInto2D 2 |> map (\[a, b] -> (a, a + b - 1))
    |> (\seeds -> foldl (\rns mp -> map (convert2 mp) rns |> concat) seeds (drop 2 lines |> parseConverters)) |> map fst |> minimum

test = [ "seeds: 79 14 55 13"
       , ""
       , "seed-to-soil map:"
       , "50 98 2"
       , "52 50 48"
       , ""
       , "soil-to-fertilizer map:"
       , "0 15 37"
       , "37 52 2"
       , "39 0 15"
       , ""
       , "fertilizer-to-water map:"
       , "49 53 8"
       , "0 11 42"
       , "42 0 7"
       , "57 7 4"
       , ""
       , "water-to-light map:"
       , "88 18 7"
       , "18 25 70"
       , ""
       , "light-to-temperature map:"
       , "45 77 23"
       , "81 45 19"
       , "68 64 13"
       , ""
       , "temperature-to-humidity map:"
       , "0 69 1"
       , "1 0 69"
       , ""
       , "humidity-to-location map:"
       , "60 56 37"
       , "56 93 4"
       ]