import           Data.Char
import qualified Data.Map  as Map

import           GridUtils

import           MUtils

type Engine = Grid Char

sumNumbers :: Engine -> Point -> Int
sumNumbers e (0, y)
    | Map.notMember (0, y) e = 0
sumNumbers e (x, y)
    | Map.notMember (x, y) e = sumNumbers e (0, y + 1)
    | isDigit (e Map.! (x, y)) && hasSymbol = [ x .. x + numberSize - 1 ] |> map (,y) |> map (e Map.!) |> readInt |> (+sumNumbers e (x+numberSize,y))
    | otherwise = sumNumbers e (x + 1, y)
    where
        numberSize = countDigits e (x, y)
        hasSymbol = combinations [x-1..x+numberSize] [y-1..y+1] |> filter (`Map.member` e) |> map (e Map.!) |> any (\s-> not (isDigit s) && s/='.')

countDigits :: Engine -> Point -> Int
countDigits e (x, y)
    | Map.notMember (x, y) e = 0
    | isDigit (e Map.! (x, y)) = 1 + (countDigits e (x + 1, y))
    | otherwise = 0

part1 :: [String] -> Int
part1 lines = gridFromList lines |> (`sumNumbers` (0,0))

part2 :: [String] -> Int
part2 lines = zipWithIndexes2 lines |> concat |> filter ((=='*') . fst) |> map snd |> map repeat |> map (zip directions2D) |> map2 (uncurry addPoints) |> filter2 (`Map.member` grid) 
    |> filter2 (isDigit . (grid Map.!)) |> map2 (getNumber grid) |> map unique |> filter ((==2) . length) |> map2 fst |> map product |> sum
        where grid = gridFromList lines

getNumber :: Engine -> Point -> (Int, Point) -- Returns a number and its starting position
getNumber e (x,y) = (readInt (backwards ++ [e Map.! (x,y)] ++ forwards), (x-length backwards, y)) where
    forwards  = [1..] |> takeWhile (\dx-> Map.member (x+dx, y) e && isDigit (e Map.! (x+dx, y))) |> map (\dx-> e Map.! (x+dx, y))
    backwards = [1..] |> takeWhile (\dx-> Map.member (x-dx, y) e && isDigit (e Map.! (x-dx, y))) |> map (\dx-> e Map.! (x-dx, y)) |> reverse

test :: [String]
test = ["467..114..", "...*......", "..35..633.", "......#...", "617*......", ".....+.58.", "..592.....", "......755.", "...$.*....", ".664.598.."]