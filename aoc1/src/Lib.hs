module Lib
    ( run
    ) where

run :: IO ()
run =
    do
      input <- getContents
      print $ solve1 input
      print $ solve2 input

mass :: Int -> Int
mass n =
    n `div` 3 - 2

solve1 :: String -> Int
solve1 s =
    sum $ map (mass . read) $ lines s

fuelRequirement :: Int -> Int
fuelRequirement m =
    let req = mass m
    in if req > 0 then
           req + fuelRequirement req
       else 0

solve2 :: String -> Int
solve2 s =
    sum $ map (fuelRequirement . read) $ lines s
