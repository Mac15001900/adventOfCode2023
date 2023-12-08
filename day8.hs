import qualified Data.Map as Map

import           MUtils

type DesertMap = Map.Map String (String, String)

parseMap :: [String] -> DesertMap
parseMap lines = filter2 (`notElem` " ()") lines |> map (splitOn '=') |> map t2fromList |> map (mapSnd (splitOn ','))
    |> map (mapSnd t2fromList) |> Map.fromList

countSteps :: DesertMap -> String -> String -> Int
countSteps _ "ZZZ" _ = 0
countSteps dMap current ('L' : ds) = 1 + countSteps dMap (dMap Map.! current |> fst) ds
countSteps dMap current ('R' : ds) = 1 + countSteps dMap (dMap Map.! current |> snd) ds

part1 :: [String] -> Int
part1 lines = countSteps (drop 2 lines |> parseMap) "AAA" (head lines |> repeat |> concat)


executeStep :: DesertMap -> Char -> String -> String
executeStep dMap _ current
    | Map.notMember current dMap = error ("executeStep from invalid location: [" ++ current ++ "]")
executeStep dMap 'L' current = dMap Map.! current |> fst
executeStep dMap 'R' current = dMap Map.! current |> snd

type State = (Int, String) -- Current position in directions list, current place

findCycleParams :: DesertMap -> String -> State -> Map.Map State Integer -> Integer -> (Integer, Integer) --Start of cycle, length of cycle
findCycleParams dMap directions (dirN, place) states moves
    | Map.member (dirN, place) states = (states Map.! (dirN, place), moves - (states Map.! (dirN, place)))
    | otherwise =
        findCycleParams dMap directions ((dirN + 1) `mod` length directions, executeStep dMap (directions !! dirN) place) (Map.insert (dirN, place) moves states) (moves + 1)

findTargetTimes :: DesertMap -> String -> String -> Integer -> Integer -> [Integer]
findTargetTimes dMap (d : ds) place moves cycle
    | moves > cycle = []
    | last place == 'Z' = moves : findTargetTimes dMap ds (executeStep dMap d place) (moves + 1) cycle
    | otherwise = findTargetTimes dMap ds (executeStep dMap d place) (moves + 1) cycle

type Target = (Integer, Integer) --Start of cycle, length of cycle

--For two given cycles produces a new cycle for each time the two arguments will overlap (or Nothing if they never do)
combine :: Integer -> Target -> Target -> Maybe Target
combine attempt (s1, l1) (s2, l2)
    | gd > 1 && (s1 `mod` gd) /= (s2 `mod` gd) = Nothing
    | (s1 - s2 + l1 * attempt) `mod` l2 == 0 = Just (s1 + l1 * attempt, lcm l1 l2)
    | otherwise = combine (attempt + 1) (s1, l1) (s2, l2)
  where
    gd = gcd l1 l2

part2 :: [String] -> Integer
part2 lines = foldl1 (\ts1 ts2 -> combinations ts1 ts2 |> map  (uncurry (combine 0)) |> removeNothing) targets |> map fst |> minimum
  where
    dMap = drop 2 lines |> parseMap
    starts = drop 2 lines |> map (head . splitOn ' ') |> filter ((== 'A') . last)
    cycles = map (\start -> findCycleParams dMap (head lines) (0, start) Map.empty 0) starts
    targets = map (\startId -> findTargetTimes dMap (head lines |> repeat |> concat) (starts !! startId) 0 (cycles !! startId |> uncurry (+)) |> map (, cycles !! startId |> snd ))
                  (indexes starts)

test :: [String]
test = [ "LR", "", "11A = (11B, XXX)", "11B = (XXX, 11Z)", "11Z = (11B, XXX)", "22A = (22B, XXX)", "22B = (22C, 22C)", "22C = (22Z, 22Z)", "22Z = (22B, 22B)", "XXX = (XXX, XXX)" ]