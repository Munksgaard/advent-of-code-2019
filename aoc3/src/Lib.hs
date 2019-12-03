module Lib
    ( run
    ) where

import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)

run :: IO ()
run = do
  s1 <- getLine
  s2 <- getLine
  let ws1 = pointsToWires $ instructionsFromLine s1
  let ws2 = pointsToWires $ instructionsFromLine s2
  print $ solve1 ws1 ws2
  print $ solve2 ws1 ws2

type Point = (Int, Int)

type Wire = (Point, Point)

add :: Point -> Point -> Point
add (x1, y1) (x2, y2) =
    (x1 + x2, y1 + y2)

len :: Wire -> Int
len ((x1, y1), (x2, y2)) =
    abs (x1 - x2) + abs (y1 - y2)

instruction :: [Char] -> (Int, Int)
instruction s =
    case s of
      ('R' : x) -> (read x, 0)
      ('D' : x) -> (0, - (read x))
      ('L' : x) -> (- (read x), 0)
      ('U' : x) -> (0, read x)
      _ -> undefined

isVertical :: Wire -> Bool
isVertical ((x1, y1), (x2, y2)) =
    x1 == x2

notOrego :: Point -> Maybe Point
notOrego p =
    if p == (0, 0) then
        Nothing
    else
        Just p

wiresCross :: Wire -> Wire -> Maybe Point
wiresCross l1@((x1, y1), (x2, y2)) l2@((x1', y1'), (x2', y2')) =
    case (isVertical l1, isVertical l2) of
      (True, True) -> if x1 == x1' && min y1 y2 <= max y1' y2' &&
                      max y1 y2 >= min y1' y2' then
                          -- error "Overlapping lines 1"
                          Nothing
                      else Nothing
      (False, False) -> if y1 == y1' && min x1 x2 <= max x1' x2' &&
                        max x1 x2 >= min x1' x2' then
                            -- error "Overlapping lines 2"
                            Nothing
                        else Nothing
      (True, False) -> if max y1 y2 >= y1' && min y1 y2 <= y1' &&
                       max x1' x2' >= x1 && min x1' x2' <= x1 then
                           notOrego (x1, y1')
                       else
                           Nothing
      (False, True) -> if max x1 x2 >= x1' && min x1 x2 <= x1' &&
                       max y1' y2' >= y1 && min y1' y2' <= y1 then
                           notOrego (x1', y1)
                       else
                           Nothing

instructionsFromLine :: String -> [Point]
instructionsFromLine s =
    scanl add (0, 0) $ map instruction $ splitOn "," s

pointsToWires :: [Point] -> [Wire]
pointsToWires ps =
    ps `zip` (tail ps)

solve1 :: [Wire] -> [Wire] -> Int
solve1 ws1 ws2 =
    foldl (\acc w -> foldl (min) acc $
                     map (\(x, y) -> abs x + abs y) $
                     mapMaybe (wiresCross w) ws2)
    maxBound
    ws1

stepsToPoint :: Point -> [Wire] -> Int
stepsToPoint _ [] = undefined
stepsToPoint p@(x, y) (w@((x1, y1), (x2, y2)) : rest) =
    if isVertical w then
        if x == x1 && y <= max y1 y2 && y >= min y1 y2 then
            abs $ y - y1
        else
            len w + stepsToPoint p rest
    else
        if y == y1 && x <= max x1 x2 && x >= min x1 x2 then
            abs $ x - x1
        else
            len w + stepsToPoint p rest

solve2 :: [Wire] -> [Wire] -> Int
solve2 ws1 ws2 =
    foldl (\acc w -> foldl (min) acc $
                     map (\p -> stepsToPoint p ws1 + stepsToPoint p ws2) $
                     mapMaybe (wiresCross w) ws2)
    maxBound
    ws1
