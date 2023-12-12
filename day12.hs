import           MUtils
import Data.Tuple

data SpringData = Broken Int | Working
    deriving ( Show, Read, Eq )


makeSpringData :: String -> [SpringData]
makeSpringData lineEnd = splitOn ',' lineEnd |> map read |> map Broken |> map (,Working) |> map t2toList |> concat |> (Working:)

countOptions :: [SpringData] -> String -> Integer
countOptions [Working] s = if '#' `elem` s then 0 else 1 --Make sure there are no more broken springs
countOptions (Broken 0:sd) ('#':s) = 0 --Contradiction: this spring should be working, but it's broken
countOptions (Broken 0:sd) (_:s) = countOptions sd s
countOptions (Broken n:sd) ('.':_) = 0 --Contradition: this spring should be broken, but it's working
countOptions (Broken n:sd) (_:s) = countOptions (Broken (n-1):sd) s
countOptions (Working:sd) ('.':s) = countOptions (Working:sd) s
countOptions (Working:sd) ('#':s) = countOptions sd ('#':s)
countOptions (Working:sd) ('?':s) = countOptions sd ('?':s) + countOptions (Working:sd) s
countOptions [Broken 0, Working] [] = 1
countOptions _ [] = 0
countOptions ds s = error ((show ds)++" | "++s)

part1:: [String] -> Integer
part1 lines = map (splitOn ' ') lines |> map t2fromList |> map (mapSnd makeSpringData) |> map swap |> map (uncurry countOptions) |> sum

part2 :: [String] -> Integer
part2 lines = map (splitOn ' ') lines |> map2 ((take 5) . repeat) |> map t2fromList |> map (mapFst (joinWith "?")) |> map (mapSnd (joinWith ",")) |> map t2toList |> map (joinWith " ") |> part1

test = ["???.### 1,1,3", ".??..??...?##. 1,1,3", "?#?#?#?#?#?#?#? 1,3,1,6", "????.#...#... 4,1,1", "????.######..#####. 1,6,5", "?###???????? 3,2,1"]