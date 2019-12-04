module Lib
    ( run
    ) where

import Data.List.Split (splitOn)
import Data.List (group)

run :: IO ()
run = putStrLn "someFunc"

type Range = (Int, Int)

digits :: Int -> [Int]
digits n =
    let aux acc 0 = acc
        aux acc n =
            aux (n `mod` 10 : acc) (n `div` 10)
    in aux [] n

hasDoubleAdjacent :: (Eq a) => [a] -> Bool
hasDoubleAdjacent [] = False
hasDoubleAdjacent [_] = False
hasDoubleAdjacent (x : y : rest) | x == y = True
                                 | otherwise = hasDoubleAdjacent (y : rest)

isNondecreasing :: (Ord a) => [a] -> Bool
isNondecreasing [] = True
isNondecreasing [_] = True
isNondecreasing (x : y : rest) = x <= y && isNondecreasing (y : rest)


isValidPassword :: Range -> Int -> Bool
isValidPassword (min, max) n =
    n >= 100000 && n <= 999999 && n >= min && n <= max && hasDoubleAdjacent ds && isNondecreasing ds
    where ds = digits n

countValidPasswords :: Range -> Int
countValidPasswords range@(min, max) =
    aux 0 min
    where aux acc n | n > max = acc
                    | otherwise =
                        if isValidPassword range n then
                            aux (acc + 1) (n + 1)
                        else
                            aux acc (n + 1)

hasDoubleAdjacentStrict :: (Eq a) => [a] -> Bool
hasDoubleAdjacentStrict xs =
    any ((==) 2 . length) $ group xs

isValidPasswordStrict :: Range -> Int -> Bool
isValidPasswordStrict (min, max) n =
    n >= 100000 && n <= 999999 && n >= min && n <= max && hasDoubleAdjacentStrict ds && isNondecreasing ds
    where ds = digits n

countValidPasswordsStrict :: Range -> Int
countValidPasswordsStrict range@(min, max) =
    aux 0 min
    where aux acc n | n > max = acc
                    | otherwise =
                        if isValidPasswordStrict range n then
                            aux (acc + 1) (n + 1)
                        else
                            aux acc (n + 1)
