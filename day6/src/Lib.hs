module Lib
    ( run
    ) where

import Data.HashMap ((!), Map, foldWithKey, insert, empty)
import qualified Data.HashMap as HashMap
import Data.List ((\\), length)

run :: IO ()
run = do
  input <- getContents
  let orbits = constructOrbits input
  print $ snd $ countOrbits orbits
  print $ orbitalTransfers "YOU" "SAN" orbits

type OrbitMap = Map String String

type OrbitCount = Map String Int

insertOrbit :: OrbitMap -> String -> OrbitMap
insertOrbit orbits s =
    let (orbitee, orbiter) = tail <$> break ((==) ')') s
    in insert orbiter orbitee orbits

constructOrbits :: String -> OrbitMap
constructOrbits s =
    foldl insertOrbit empty $ lines s

orbitCounts :: OrbitMap -> String -> OrbitCount -> (OrbitCount, Int)
orbitCounts _ "COM" countMap = (countMap, 0)
orbitCounts orbits orbiter countMap =
    case HashMap.lookup orbiter countMap of
      Just count -> (countMap, count)
      Nothing ->
          let orbitee = orbits ! orbiter
              (countMap', count) = orbitCounts orbits orbitee countMap
          in (insert orbiter (count + 1) countMap', count + 1)

countOrbits :: OrbitMap -> (OrbitCount, Int)
countOrbits orbits =
    foldWithKey helper (empty, 0) orbits
    where
      helper orbiter _ (countMap, acc) =
          (+) acc <$> orbitCounts orbits orbiter countMap

pathToCom :: String -> OrbitMap -> [ String ]
pathToCom "COM" orbits = ["COM"]
pathToCom x orbits =
    x : (pathToCom (orbits ! x) orbits)

orbitalTransfers :: String -> String -> OrbitMap -> Int
orbitalTransfers from to orbits =
    let p1 = pathToCom from orbits
        p2 = pathToCom to orbits
    in length (p1 \\ p2) + length (p2 \\ p1) - 2
