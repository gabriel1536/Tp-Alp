{-# OPTIONS -XRecordWildCards #-}
{-# OPTIONS -XOverloadedStrings #-}

--
module Main where

import System.IO
import System.IO.Error
import Data.Text (pack, unpack,strip)
import Data.List
import Data.Char
import Data.Ord
import Data.List.Split
import System.Console.Haskeline
import Failable 
import Control.Exception (try)
import Text.PrettyPrint.HughesPJ (render)

import Common
import Parser
import PrettyPrinter

-- data FSM = FSM { name        :: String
--                , alphabet    :: [String]
--                , states      :: [String]
--                , iState      :: [String]
--                , fState      :: [String]
--                , transitions :: [String]
--                } deriving (Show)

main :: IO ()
main = main2 [(Fsm {name = "First", alphabet = [], states = [], iState = "", fState = [], transitions = []})]

main2 :: FSM -> IO ()
main2 fsm@(x)= do
    putStr $ "> "
    _line <- getLine
    let line = unpack (strip $ pack _line) in
        case getOnlyCommand line of 
            ":q" -> return ()
            ":print_fsm" -> do -- print every fsm (fix)
                putStrLn $ ppFsm fsm
                main2 fsm
            ":pp_parsed" -> do -- print parsed fsm file (used for debugging)
                args <- getArgs 0 line    
                checkForArguments fsm args
                ppParsedWork fsm args
            ":help" -> do 
                putStrLn $ render $ (ppHelpCommands)
                main2 fsm
            ":create_fsm" -> do -- :create_fsm nameOfNewFsm
                args <- getArgs 1 line    
                checkForArguments fsm args
                createFsmWork fsm args
            ":addStateTo" -> do -- :addStateTo FSM nameOfNewState
                args <- getArgs 2 line
                checkForArguments fsm args
                addStateToWork fsm args
            ":addIStateTo" -> do -- :addIStateTo FSM nameOfExistingState
                args <- getArgs 2 line
                checkForArguments fsm args
                addIStateToWork fsm args
            ":addFStateTo" -> do -- :addFStateTo FSM nameOfExistingState
                args <- getArgs 2 line
                checkForArguments fsm args
                addFStateToWork fsm args
            ":addTransTo" -> do -- :addTransTo FSM nameOfExistingState1 nameOfExistingState2
                args <- getArgs 3 line
                checkForArguments fsm args
                addTransToWork fsm args
            _ -> do 
                unknComm
                main2 fsm




--------------------------------------------------
--------------------------------------------------
              -- get/set funcs --

fsmNames :: FSM -> [String]
fsmNames [] = []
fsmNames (x:xs) = (name x) : (fsmNames xs)

getJustString :: Maybe [String] -> [String]
getJustString (Just a) = a
getJustString Nothing = []

getArgs :: Int -> String -> IO (Maybe [String])
getArgs expectedArgs line = 
    let args = (splitOn " " line) in
        if ((length args) - 1 < expectedArgs) then (return Nothing) else -- (-1) means 'ignoring :command'
            case length args of -- not the greatest logic ever
                0 -> return Nothing
                1 -> return Nothing
                2 -> if ((args !! 1) == "") then (return Nothing) else (return (Just (tail args)))
                _ -> return (Just (tail args))

getOnlyCommand :: String -> String
getOnlyCommand s = (splitOn " " s) !! 0

getRestOfLine :: String -> Maybe String
getRestOfLine s = case (splitOn " " s) of
                  [x] -> Nothing
                  [x,y] -> Just y

unknComm :: IO ()
unknComm = putStrLn $ "Didn't get that, type ':help' for known commands"

              -- get/set funcs --
--------------------------------------------------
--------------------------------------------------





--------------------------------------------------
--------------------------------------------------
              -- control funcs --

checkForArguments :: FSM -> Maybe [String] -> IO ()
checkForArguments s Nothing = do
    putStrLn "Not enough arguments!"
    main2 s
checkForArguments _ _ = putStr "" -- fix? I guess... It works though

missingArgsFunc :: SomeException -> IO ()
missingArgsFunc ex = if (isInfixOf "index too large" (show ex)) then (putStrLn "Error: Missing args!") 
                    else (putStrLn ("Caught Exception: " ++ show ex))

checkForExistingStates :: FSM -> String -> Bool
checkForExistingStates fsm state = let states = fsmNames fsm in
    foldl (||) False (map (\sname -> sname == state) states) 

stateInList :: [String] -> String -> Bool
stateInList states st = foldl (||) False (map (\s -> s == st) states)

              -- control funcs --
--------------------------------------------------
--------------------------------------------------




--------------------------------------------------
--------------------------------------------------
              -- state mod funcs --

addStateTo :: FSM -> String -> String -> FSM
addStateTo [] _ _ = []
addStateTo (x:xs) fsmName newStateName = let compareName = (name x) == fsmName in
    case compareName of
        True -> let x' = x { states = newStateName : (states x) } in
            [x'] ++ xs
        False -> [x] ++ (addStateTo xs fsmName newStateName)

addIStateTo :: FSM -> String -> String -> (String, FSM)
addIStateTo [] _ _ = ("",[])
addIStateTo (x:xs) fsmName newIStateName = let compareName = (name x) == fsmName in
    case compareName of
        True -> if (stateInList (states x) newIStateName) then
            (let x' = x { iState = newIStateName } in
                ("Ok!" ,[x'] ++ xs)) else ("Not a valid state for {" ++ name x ++ "}", (x:xs))
        False -> ("",[x] ++ snd (addIStateTo xs fsmName newIStateName))

addFStateTo :: FSM -> String -> String -> (String, FSM)
addFStateTo [] _ _ = ("",[])
addFStateTo (x:xs) fsmName newFStateName = let compareName = (name x) == fsmName in
    case compareName of
        True -> if (stateInList (states x) newFStateName) then
            (case (stateInList (fState x) newFStateName) of
                True -> ("final state already assigned for {" ++ name x ++ "}", (x:xs))
                False -> 
                    (let x' = x { fState = newFStateName : (fState x) } in
                        ("Ok!" ,[x'] ++ xs))) else ("Not a valid state for {" ++ name x ++ "}", (x:xs))
        False -> ("",[x] ++ snd (addFStateTo xs fsmName newFStateName))

addFsmByName :: String -> FSM -> Maybe FSM
addFsmByName fsmName fsm@(xs) = if (fsmName == "") then Nothing else if (notElem fsmName (map (\x -> name x) xs)) then (Just ((Fsm {name = fsmName, alphabet = [], states = [], iState = "", fState = [], transitions = []}):xs)) else (Nothing)
              
              -- state mod funcs --
--------------------------------------------------
--------------------------------------------------




--------------------------------------------------
--------------------------------------------------
                -- aux funcs --
ppParsedWork :: FSM -> Maybe [String] -> IO ()
ppParsedWork fsm args = do
    fsmcode <- try (readFile $ (getJustString args) !! 0) :: IO (Either SomeException String)
    case fsmcode of -- checking for existing file
        Left ex -> do
            missingArgsFunc ex
            main2 fsm
        Right content -> do
            case parseComm content of -- checking for parser
                Ok m -> do 
                        putStrLn $ render $ (ppComm m)
                Error r -> putStrLn $ r
            main2 fsm

addStateToWork :: FSM -> Maybe [String] -> IO ()
addStateToWork fsm args = 
    case (checkForExistingStates fsm $ (getJustString args) !! 0) of
        False -> do
            putStrLn $ "Not a valid state for FSM {" ++ ((getJustString args) !! 0) ++ "}"
            main2 fsm
        True -> 
            let newState = addStateTo fsm ((getJustString args) !! 0) ((getJustString args) !! 1) in
                do putStrLn "Ok!"
                   main2 newState   

addIStateToWork :: FSM -> Maybe [String] -> IO ()
addIStateToWork fsm args = 
    case (checkForExistingStates fsm $ (getJustString args) !! 0) of
        False -> do
            putStrLn $ "Not a valid state for FSM {" ++ ((getJustString args) !! 0) ++ "}"
            main2 fsm
        True -> 
            let (str, newState) = addIStateTo fsm ((getJustString args) !! 0) ((getJustString args) !! 1) in
                do putStrLn str
                   main2 newState

addFStateToWork :: FSM -> Maybe [String] -> IO ()
addFStateToWork fsm args = 
    case (checkForExistingStates fsm $ (getJustString args) !! 0) of
        False -> do
            putStrLn $ "Not a valid state for FSM {" ++ ((getJustString args) !! 0) ++ "}"
            main2 fsm
        True -> 
            let (str, newState) = addFStateTo fsm ((getJustString args) !! 0) ((getJustString args) !! 1) in
                do putStrLn str
                   main2 newState

addTransToWork :: FSM -> Maybe [String] -> IO ()
addTransToWork fsm args = 
    case ((checkForExistingStates fsm $ (getJustString args) !! 0) && (checkForExistingStates fsm $ (getJustString args) !! 1)) of
        False -> do
            putStrLn $ "Not a valid pair of states for the FSM {" ++ ((getJustString args) !! 0) ++ "}"
            main2 fsm
        True -> 
            let (str, newState) = addFStateTo fsm ((getJustString args) !! 0) ((getJustString args) !! 1) in
                do putStrLn str
                    main2 newState


createFsmWork :: FSM -> Maybe [String] -> IO ()
createFsmWork fsm args = do
    fsmName <- try (return ((getJustString args) !! 0)) :: IO (Either SomeException String)
    case fsmName of
        Left ex -> do
            missingArgsFunc ex
            main2 fsm
        Right fname ->
            let s' = addFsmByName fname fsm in
                case s' of
                    Just newState -> do
                        putStr "Ok! Name: "
                        putStrLn $ name (newState !! 0)
                        main2 newState
                    Nothing -> do
                        putStrLn "Invalid Name. Try again."
                        main2 fsm

                -- aux funcs --   
--------------------------------------------------
--------------------------------------------------