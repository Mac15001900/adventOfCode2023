import           Data.Char
import qualified Data.Map  as Map

import           MUtils

hash :: String -> Integer
hash s = map ord s |> map toInteger |> foldl (\new old -> (new + old) * 17 `mod` 256) 0

part1 :: [String] -> Integer
part1 lines = head lines |> splitOn ',' |> map hash |> sum

type Lens = (String, Integer) --Label and focal strength
type Boxes = Map.Map Integer [Lens] --Map from box number to list of lenses in that box. Head of this list is the 'back' of the set of lenses.
data Command = Remove String | Add Lens
    deriving ( Show, Read, Eq, Ord ) --Remove [label], or Add [lens]

parseCommand :: String -> Command
parseCommand s
    | '-' `elem` s = Remove (init s)
    | '=' `elem` s = splitOn '=' s |> \[a, b] -> Add (a, read b)

executeCommand :: Boxes -> Command -> Boxes
executeCommand boxes (Remove label) = Map.adjust (filter ((/= label) . fst)) (hash label) boxes
executeCommand boxes (Add lens@(label, s)) = Map.adjust (\list -> if any ((== label) . fst) list then replaceIf ((== label) . fst) lens list else lens : list) (hash label) boxes

focusPower :: Boxes -> Integer
focusPower boxes = boxes |> Map.map (map snd) |> Map.map reverse |> Map.map zipWithIndexes |> Map.map (map (\(f, i) -> f * (toInteger i + 1))) |> Map.toList
    |> map (mapFst (+ 1)) |> map (mapSnd sum) |> map (uncurry (*)) |> sum

part2 :: [String] -> Integer
part2 lines = head lines |> splitOn ',' |> map parseCommand |> foldl executeCommand (zip [ 0 .. 255 ] (repeat []) |> Map.fromList) |> focusPower

test = [ "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" ]