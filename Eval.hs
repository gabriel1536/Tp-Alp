{-# OPTIONS -XRecordWildCards #-}
{-# OPTIONS -XOverloadedStrings #-}

module Eval where

import Common
import Data.Text (pack, unpack,strip)
import Data.List
import Data.List.Split

evalWord :: Fsm -> String -> IO (String)
evalWord a s = 
  let (h:t) = splitOn "" s in
    if fst (evalWordAux a t (iState a)) then (return "ok!") else (return "no.")

evalWordAux :: Fsm -> [String] -> String -> (Bool, String)
evalWordAux a (x:[]) s = checkLast a x s
evalWordAux a (x:xs) s = 
  case checkWord a x s of
    (True, ns) -> evalWordAux a xs ns
    (False, _) -> (False, "")

checkWord :: Fsm -> String -> String -> (Bool, String)
checkWord a w st = 
  let n = filter (\(from, to, word) -> (from == st) && (word == w)) (transitions a) in
    case (length n) of
      1 -> let (_, ns, _) = n !! 0 in (True, ns)
      _ -> (False, "")

checkLast :: Fsm -> String -> String -> (Bool, String)
checkLast a w fs = 
  case (checkWord a w fs) of
    (True, ns) -> (elem ns (fState a), "")
    (False, _) -> (False, "")