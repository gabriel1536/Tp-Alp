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
                ppParsedWork fsm (getJustString args)
            ":load" -> do
                args <- getArgs 0 line    
                checkForArguments fsm args
                loadFileWork fsm (getJustString args)
            ":help" -> do 
                putStrLn $ render ppHelpCommands
                main2 fsm
            ":create_fsm" -> do -- :create_fsm nameOfNewFsm
                args <- getArgs 1 line    
                checkForArguments fsm args
                createFsmWork fsm (getJustString args)
            ":addStateTo" -> do -- :addStateTo FSM nameOfNewState
                args <- getArgs 2 line
                checkForArguments fsm args
                addStateToWork fsm (getJustString args)
            ":addIStateTo" -> do -- :addIStateTo FSM nameOfExistingState
                args <- getArgs 2 line
                checkForArguments fsm args
                addIStateToWork fsm (getJustString args)
            ":addFStateTo" -> do -- :addFStateTo FSM nameOfExistingState
                args <- getArgs 2 line
                checkForArguments fsm args
                addFStateToWork fsm (getJustString args)
            ":addTransTo" -> do -- :addTransTo FSM nameOfExistingState1 nameOfExistingState2 alphElem
                args <- getArgs 4 line
                checkForArguments fsm args
                addTransToWork fsm (getJustString args)
            ":determineThis" -> do -- :determineThis FSM
                args <- getArgs 1 line
                checkForArguments fsm args
                determineWork fsm (getJustString args)
            _ -> do 
                unknComm
                main2 fsm





--------------------------------------------------
--------------------------------------------------
              -- get/set funcs --

replaceFSM :: FSM -> Fsm -> FSM
replaceFSM [] _ = []
replaceFSM (x:xs) fsm = let compareName = (name x) == (name fsm) in
    case compareName of
        True -> (fsm:xs)
        False -> [x] ++ (replaceFSM xs fsm)

fsmNames :: FSM -> [String]
fsmNames [] = []
fsmNames (x:xs) = (name x) : (fsmNames xs)

getFsmByName :: FSM -> String -> Fsm
getFsmByName [] _ = Fsm {name = "", alphabet = [], states = [], iState = "", fState = [], transitions = []} 
getFsmByName (x:xs) fsmName = if ((name x) == fsmName) 
    then x
    else (getFsmByName xs fsmName)

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

checkForExistingFSM :: FSM -> String -> Bool
checkForExistingFSM fsm fsmName = let names = fsmNames fsm in
    foldl (||) False (map (\sname -> sname == fsmName) names) 

checkForExistingStates :: FSM -> String -> String -> Bool
checkForExistingStates fsm fsmName state = let fsmstates = states (getFsmByName fsm fsmName) in
    foldl (||) False (map (\sname -> sname == state) fsmstates) 

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

addTransTo :: FSM -> String -> String -> String -> String -> FSM
addTransTo [] _ _ _ _ = []
addTransTo (x:xs) fsmName stateFrom stateTo value = 
    let compareName = (name x) == fsmName in
        case compareName of
            True -> let x' = x { transitions = (stateFrom, stateTo, value) : (transitions x) , alphabet = cleanDupes (value : (alphabet x))} in
                [x'] ++ xs
            False -> [x] ++ (addTransTo xs fsmName stateFrom stateTo value)

addFsmByName :: String -> FSM -> Maybe FSM
addFsmByName fsmName fsm@(xs) = if (fsmName == "") then Nothing else if (notElem fsmName (map (\x -> name x) xs)) then (Just ((Fsm {name = fsmName, alphabet = [], states = [], iState = "", fState = [], transitions = []}):xs)) else (Nothing)
              

-- data Fsm = Fsm { name        :: Variable
--                , alphabet    :: Alph
--                , states      :: States
--                , iState      :: String
--                , fState      :: FState
--                , transitions :: Transitions 
--                } deriving (Show)

--possible alphabet

cleanDupes :: [String] -> [String]
cleanDupes [] = []
cleanDupes (x:xs) = [x] ++ (cleanDupes (filter (\z -> z /= x) xs))

transLookup :: Transitions -> String -> String -> String
transLookup [] _ _ = ""
transLookup ((from, to, word):xs) st w = case (from == st && word == w) of
    True -> to ++ "-" ++ (transLookup xs st w) 
    False -> transLookup xs st w

combStates :: Transitions -> [String] -> String -> String
combStates _ [] _ = ""
combStates trans (s:stName) (w) = (transLookup trans s w) ++ "-" ++ (combStates trans stName w)

cleanLookup :: String -> String 
cleanLookup [] = ""
cleanLookup (x:xs) = [x] ++ (cleanLookup (filter (\z -> z /= x) xs))

getNextSName :: String -> Transitions -> String -> (String, Transitions)
getNextSName n trans w = let st = cleanLookup (combStates trans (sort (splitOn "-" n)) w) in
    (st, [(n, st, w)])

getNextSNames :: [String] -> [String] -> Transitions -> String -> ([String], Transitions)
getNextSNames d [] tr w = (d , tr)
getNextSNames (done) (g:got) trans w = let (s, t) = getNextSName g trans w in
    if (stateInList done s) then
        (done, t)
    else
        getNextSNames (g:done) (got ++ [s]) t w

g :: [String] -> Transitions -> [String] -> ([String], Transitions)
g st tr [] = (st, tr)
g st tr (w:ws) = let (s, t) = getNextSNames [] st tr w in
    g (st ++ s) (tr ++ t) ws


determineWorkAux :: Fsm -> Fsm
determineWorkAux fsm = 
    let (st, trans) = g [(iState fsm)] (transitions fsm) (alphabet fsm)
    in
        fsm { states = st, transitions = trans }
    





              -- state mod funcs --
--------------------------------------------------
--------------------------------------------------




--------------------------------------------------
--------------------------------------------------
                -- aux funcs --
ppParsedWork :: FSM -> [String] -> IO ()
ppParsedWork fsm args = do
    fsmcode <- try (readFile $ args !! 0) :: IO (Either SomeException String)
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

addStateToWork :: FSM -> [String] -> IO ()
addStateToWork fsm args = let fsmName = args !! 0
                              state1  = args !! 1
    in
    case ((checkForExistingFSM fsm fsmName), (checkForExistingStates fsm fsmName state1)) of
        (False, _) -> do
            putStrLn $ "FSM {" ++ fsmName ++ "} not found."
            main2 fsm
        (True, True) -> do
            putStrLn $ "State {" ++ state1 ++ "} already exists!."
            main2 fsm
        (True, False) -> 
            let newState = addStateTo fsm fsmName state1 in
                do putStrLn "Ok!"
                   main2 newState   

addIStateToWork :: FSM -> [String] -> IO ()
addIStateToWork fsm args = let fsmName = args !! 0
                               state1  = args !! 1
    in
    case ((checkForExistingFSM fsm fsmName), (checkForExistingStates fsm fsmName state1)) of
        (False, _) -> do
            putStrLn $ "FSM {" ++ fsmName ++ "} not found."
            main2 fsm
        (True, False) -> do
            putStrLn $ "Not a valid state for FSM {" ++ fsmName ++ "}"
            main2 fsm
        (True, True) -> 
            let (str, newState) = addIStateTo fsm fsmName state1 in
                do putStrLn str
                   main2 newState

addFStateToWork :: FSM -> [String] -> IO ()
addFStateToWork fsm args = let fsmName = args !! 0
                               state1  = args !! 1
    in
    case ((checkForExistingFSM fsm fsmName), (checkForExistingStates fsm fsmName state1)) of
        (False, _) -> do
            putStrLn $ "FSM {" ++ fsmName ++ "} not found."
            main2 fsm
        (True, False) -> do
            putStrLn $ "Not a valid state for FSM {" ++ fsmName ++ "}"
            main2 fsm
        (True, True) -> 
            let (str, newState) = addFStateTo fsm fsmName state1 in
                do putStrLn str
                   main2 newState

addTransToWork :: FSM -> [String] -> IO ()
addTransToWork fsm args = let fsmName = args !! 0
                              state1  = args !! 1
                              state2  = args !! 2
                              value   = args !! 3 
    in
    case ((checkForExistingFSM fsm fsmName), ((checkForExistingStates fsm fsmName state1) && (checkForExistingStates fsm fsmName state2))) of
        (False, _) -> do 
            putStrLn $ "FSM {" ++ (args !! 0) ++ "} not found."
            main2 fsm
        (True, False) -> do
            putStrLn $ "Not a valid pair of states for the FSM {" ++ (args !! 0) ++ "}"
            main2 fsm
        (True,True) -> do
            let newState = addTransTo fsm fsmName state1 state2 value
            putStrLn $ "OK!, new trans: (" ++ state1 ++ state2 ++ value ++ ")."
            main2 newState

createFsmWork :: FSM -> [String] -> IO ()
createFsmWork fsm args = do
    fsmName <- try (return (args !! 0)) :: IO (Either SomeException String)
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

determineWork :: FSM -> [String] -> IO ()
determineWork fsm args = do
    let fsmName = args !! 0
    case (checkForExistingFSM fsm fsmName) of
        False -> do 
            putStrLn $ "FSM {" ++ (args !! 0) ++ "} not found."
            main2 fsm
        True -> do
            let newState = determineWorkAux (getFsmByName fsm fsmName)
            putStrLn "OK!, new fsm:"
            putStrLn $ ppFsm newState
            main2 (replaceFSM fsm newState)

loadFileWork :: FSM -> [String] -> IO ()

                -- aux funcs --   
--------------------------------------------------
--------------------------------------------------