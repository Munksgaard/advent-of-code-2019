module Lib
    ( run
    ) where

import Data.Char (digitToInt)
import Control.Exception
import Data.Vector.Mutable (write)
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as Vector
import Data.List.Split

run :: IO ()
run = do
  input <- getLine
  let vs = toVec input
  vs' <- eval 0 vs
  return ()

data ComputerState
    = Running (Int, Vector Int)
    | Terminated (Vector Int)
    | Errored String
      deriving Show

type MemoryPosition = Int

data ParameterMode
    = Position
    | Immediate
      deriving (Show, Eq)

data Instruction
    = Plus ParameterMode ParameterMode
    | Multiply ParameterMode ParameterMode
    | Input
    | Output ParameterMode
    | JumpIfTrue ParameterMode ParameterMode
    | JumpIfFalse ParameterMode ParameterMode
    | LessThan ParameterMode ParameterMode
    | Equals ParameterMode ParameterMode
    | Terminate
      deriving (Show, Eq)

toVec :: String -> Vector Int
toVec s =
    Vector.fromList $ map read $ splitOn "," s

parseInt :: Int -> Maybe Instruction
parseInt n =
    case n `mod` 100 of
      1 -> do p1 <- parameterMode $ n `div` 100 `mod` 10
              p2 <- parameterMode $ n `div` 1000 `mod` 10
              return $ Plus p1 p2
      2 -> do p1 <- parameterMode $ n `div` 100 `mod` 10
              p2 <- parameterMode $ n `div` 1000 `mod` 10
              return $ Multiply p1 p2
      3 -> return Input
      4 -> do p <- parameterMode $ n `div` 100 `mod` 10
              return $ Output p
      5 -> do p1 <- parameterMode $ n `div` 100 `mod` 10
              p2 <- parameterMode $ n `div` 1000 `mod` 10
              return $ JumpIfTrue p1 p2
      6 -> do p1 <- parameterMode $ n `div` 100 `mod` 10
              p2 <- parameterMode $ n `div` 1000 `mod` 10
              return $ JumpIfFalse p1 p2
      7 -> do p1 <- parameterMode $ n `div` 100 `mod` 10
              p2 <- parameterMode $ n `div` 1000 `mod` 10
              return $ LessThan p1 p2
      8 -> do p1 <- parameterMode $ n `div` 100 `mod` 10
              p2 <- parameterMode $ n `div` 1000 `mod` 10
              return $ Equals p1 p2
      99 -> return $ Terminate
      _ -> fail "Invalid instruction"
    where
      parameterMode :: Int -> Maybe ParameterMode
      parameterMode 0 = return Position
      parameterMode 1 = return Immediate
      parameterMode _ = fail "Invalid parameter mode"

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left

getParam :: Int -> ParameterMode -> Vector Int -> Maybe Int
getParam i mode vs =
    case mode of
      Position -> vs !? i
      Immediate -> return i

eval' :: Int -> Vector Int -> IO ComputerState
eval' ip vs =
    case parseInt $ vs ! ip of
          Just (Plus p1 p2) ->
              case (getParam (vs ! (ip + 1)) p1 vs,
                    getParam (vs ! (ip + 2)) p2 vs) of
                (Just x, Just y) ->
                      let vs' = Vector.modify (\v -> write v (vs ! (ip + 3)) $ x + y) vs
                      in return $ Running (ip + 4, vs')

                _ -> return $ Errored "Invalid parameters"

          Just (Multiply p1 p2) ->
              case (getParam (vs ! (ip + 1)) p1 vs,
                    getParam (vs ! (ip + 2)) p2 vs) of
                (Just x, Just y) ->
                      let vs' = Vector.modify (\v -> write v (vs ! (ip + 3)) $ x * y) vs
                      in return $ Running (ip + 4, vs')

                _ -> return $ Errored "Invalid parameters"

          Just Input -> do
              c <- getChar
              let vs' = Vector.modify (\v -> write v (vs ! (ip + 1)) $ digitToInt c) vs
              return $ Running (ip + 2, vs')

          Just (Output p) ->
              case getParam (vs ! (ip + 1)) p vs of
                Just v -> do
                    putStrLn $ show $ v
                    return $ Running (ip + 2, vs)
                Nothing ->
                    return $ Errored "Invalid parameters"

          Just (JumpIfTrue p1 p2) ->
              case getParam (vs ! (ip + 1)) p1 vs of
                Just x | x /= 0 ->
                    case getParam (vs ! (ip + 2)) p2 vs of
                      Just y -> return $ Running (y, vs)
                      Nothing -> return $ Errored "Invalid parameters"
                       | otherwise ->
                           return $ Running (ip + 3, vs)
                Nothing ->
                    return $ Errored "Invalid parameters"

          Just (JumpIfFalse p1 p2) ->
              case getParam (vs ! (ip + 1)) p1 vs of
                Just x | x == 0 ->
                    case getParam (vs ! (ip + 2)) p2 vs of
                      Just y -> return $ Running (y, vs)
                      Nothing -> return $ Errored "Invalid parameters"
                       | otherwise ->
                           return $ Running (ip + 3, vs)
                Nothing ->
                    return $ Errored "Invalid parameters"

          Just (LessThan p1 p2) ->
              case (getParam (vs ! (ip + 1)) p1 vs,
                    getParam (vs ! (ip + 2)) p2 vs) of
                (Just x, Just y) ->
                      let res = if x < y then 1 else 0
                          vs' = Vector.modify (\v -> write v (vs ! (ip + 3)) res) vs
                      in return $ Running (ip + 4, vs')
                _ -> return $ Errored "Invalid parameters"

          Just (Equals p1 p2) ->
              case (getParam (vs ! (ip + 1)) p1 vs,
                    getParam (vs ! (ip + 2)) p2 vs) of
                (Just x, Just y) ->
                      let res = if x == y then 1 else 0
                          vs' = Vector.modify (\v -> write v (vs ! (ip + 3)) res) vs
                      in return $ Running (ip + 4, vs')

                _ -> return $ Errored "Invalid parameters"

          Just Terminate -> return $ Terminated vs
          x -> return $ Errored $ "undefined instruction: " ++ show x ++ " : " ++ show (vs ! ip) ++ " : " ++ show ip

eval :: Int -> Vector Int -> IO (Vector Int)
eval ip vs = do
  newState <- eval' ip vs
  case newState of
    Running (ip', vs') -> eval ip' vs'
    Terminated vs' -> return vs'
    Errored err -> error err



--     case vs !? ip of
--       Just instruction' ->
--           case parse instruction' of
--             Just instruction ->
--                 helper instruction
--             Nothing -> Errored "Invalid instruction"
--       Nothing ->
--           Errored "Invalid ip"
--     where
--       helper (Plus p1 p2) =
--           if Vector.length vs - 4
--           case (getParam

--     case parse instruction' of
--       Just instr -> Errored "Invalid
--      case instruction of
--           Multiply p1 p2 -> Just Terminated
--           _ -> Nothing)


-- -- do

    --           if vs ! ip < Vector.length vs - 4 then
    --               let x = vs ! (vs ! (ip + 1))
    --                   y = vs ! (vs ! (ip + 2))
    --                   result = x + y
    --                   vs' = Vector.modify (\v -> write v (vs ! (ip + 3)) result) vs
    --               in Running (ip + 4, vs')
    --           else
    --               Errored "Cannot execute instruction 1, not enough space"
    --       2 -> if vs ! ip < Vector.length vs - 4 then
    --                let x = vs ! (vs ! (ip + 1))
    --                    y = vs ! ( vs ! (ip + 2))
    --                    result = x * y
    --                    vs' = Vector.modify (\v -> write v (vs ! (ip + 3)) result) vs
    --                in Running (ip + 4, vs')
    --            else
    --               Errored "Cannot execute instruction 1, not enough space"
    --       99 -> Terminated vs
    --       n -> Errored $ "undefined instruction: " ++ show n
    -- else
    --     Errored "invalid instruction pointer"


-- eval :: Int -> Vector Int -> Vector Int
-- eval ip vs =
--     case eval' ip vs of
--       Running (ip', vs') -> eval ip' vs'
--       Terminated vs' -> vs'
--       Errored err -> error err
