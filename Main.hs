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
main = main2 ([], [])--[(Fsm {name = "First", alphabet = [], states = [], iState = "", fState = [], transitions = []})])

main2 :: (VEnv, FSM) -> IO ()
main2 vfsm@(x)= do
    let fsm = snd vfsm
    putStr $ "> "
    _line <- getLine
    let line = unpack (strip $ pack _line) in
        case getOnlyCommand line of 
            ":q" -> return ()
            ":print_fsm" -> do -- print every fsm (fix)
                putStrLn $ ppFsm fsm
                main2 ([], fsm)
            ":pp_parsed" -> do -- print parsed fsm file (used for debugging)
                args <- getArgs 0 line    
                checkForArguments fsm args
                newfsm <- ppParsedWork fsm (getJustString args)
                main2 ([], newfsm)
            ":load" -> do
                args <- getArgs 0 line    
                checkForArguments fsm args
                newfsm <- loadFileWork fsm (getJustString args)
                main2 ([], newfsm)
            ":help" -> do 
                putStrLn $ render ppHelpCommands
                main2 (fst vfsm, fsm)
            ":create_fsm" -> do -- :create_fsm nameOfNewFsm
                args <- getArgs 1 line    
                checkForArguments fsm args
                newfsm <- createFsmWork fsm (getJustString args)
                main2 ([], newfsm)
            ":addStateTo" -> do -- :addStateTo FSM nameOfNewState
                args <- getArgs 2 line
                checkForArguments fsm args
                newfsm <- addStateToWork fsm (getJustString args)
                main2 ([], newfsm)
            ":addIStateTo" -> do -- :addIStateTo FSM nameOfExistingState
                args <- getArgs 2 line
                checkForArguments fsm args
                newfsm <- addIStateToWork fsm (getJustString args)
                main2 ([], newfsm)
            ":addFStateTo" -> do -- :addFStateTo FSM nameOfExistingState
                args <- getArgs 2 line
                checkForArguments fsm args
                newfsm <- addFStateToWork fsm (getJustString args)
                main2 ([], newfsm)
            ":addTransTo" -> do -- :addTransTo FSM nameOfExistingState1 nameOfExistingState2 alphElem
                args <- getArgs 4 line
                checkForArguments fsm args
                newfsm <- addTransToWork fsm (getJustString args)
                main2 ([], newfsm)
            ":determineThis" -> do -- :determineThis FSM
                args <- getArgs 1 line
                checkForArguments fsm args
                newfsm <- determineWork fsm (getJustString args)
                main2 ([], newfsm)
            _ -> do 
                unknComm
                main2 (fst vfsm, fsm)





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
    main2 ([], s)
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
    True -> to ++ (transLookup xs st w) 
    False -> transLookup xs st w

transLookupAlph :: Transitions -> String -> [String] -> Transitions
transLookupAlph _ _ [] = []
transLookupAlph tr state (word:words) = let newS = intercalate "-" (map (\x -> [x]) (transLookup tr state word)) in
    if (newS /= "") then
        [(state, newS, word)] ++ (transLookupAlph tr state words)    
    else (transLookupAlph tr state words)  

transLookupGam :: Transitions -> [String] -> String -> String
transLookupGam _ [] word = []
transLookupGam tr (s:states) word = let newS = transLookup tr s word in
    newS ++ transLookupGam tr states word

-- transLookupBet :: Transitions -> [String] -> [String] -> Transitions
-- transLookupBet _ [] _ = []
-- transLookupBet tr (x:xs) (words) = (transLookupAlph tr x words) ++ (transLookupBet tr xs words)

addnewState :: [String] -> [String] -> [String]
addnewState old [] = old
addnewState old (n:new) = if (stateInList old n) then (addnewState old new) else (addnewState (old ++ [n]) new)

get3 :: [String] -> [String] -> Transitions -> Transitions -> Int -> (Transitions, [String])
--get3 old [] _ tr _ = (tr, old)
get3 old words tr trnew n = 
    if ((length old) > n) then
        let 
            (trs, sts) = get2 (old !! n) words tr
        in
            get3 (addnewState old sts) words tr (trnew ++ trs) (n+1)
    else
        (trnew, old)

get2 :: String -> [String] -> Transitions -> (Transitions, [String])
get2 state [] oldTr = ([], [])
get2 state (w:words) oldTr = 
    let 
        newStates = intercalate "-" (map (\x -> [x]) (transLookupGam oldTr (splitOn "-" state) w))
        newtrans = (state, newStates, w)
        --newtrans = transLookupBet oldTr (splitOn "-" state) words 
        --newStates = map (\(x,y,z)-> y) newtrans
    in
        if (newStates /= "") then
            let (ret1, ret2) = get2 state words oldTr in
                ([newtrans] ++ ret1 , [newStates] ++ ret2)
        else
            get2 state words oldTr

determineWorkAux :: Fsm -> IO (Fsm)
determineWorkAux old =
    let trans = transitions old
        alph = alphabet old
        fstates = fState old
        init = iState old
        (newTrans, newStates) = get3 [init] alph trans [] 0
        fStates = getFs fstates trans newTrans 
    in
        return $ Fsm {name = name old, alphabet = alph, states = newStates, iState = init, fState = fStates, transitions = newTrans}

getFs :: States -> Transitions -> Transitions -> States
getFs [] _ _ = []
getFs (x:xs) oldtr newtr = 
    let 
        fts = cleanDupes [c | (a,b,c) <- oldtr, x == b] 
        sfs = cleanDupes [b | (a,b,c) <- newtr, elem c fts]
    in
        sfs ++ (getFs xs oldtr newtr)

-- determineWorkAux :: Fsm -> IO (Fsm)
-- determineWorkAux fsm = return $ get1 fsm
    

-- data Fsm = Fsm { name        :: Variable
--                , alphabet    :: Alph
--                , states      :: States
--                , iState      :: String
--                , fState      :: FState
--                , transitions :: Transitions 
--                } deriving (Show)



              -- state mod funcs --
--------------------------------------------------
--------------------------------------------------




--------------------------------------------------
--------------------------------------------------
                -- aux funcs --
ppParsedWork :: FSM -> [String] -> IO (FSM)
ppParsedWork fsm args = do
    fsmcode <- try (readFile $ args !! 0) :: IO (Either SomeException String)
    case fsmcode of -- checking for existing file
        Left ex -> do
            missingArgsFunc ex
            return fsm
        Right content -> do
            case parseComm content of -- checking for parser
                Ok m -> do 
                        putStrLn $ render $ (ppComm m)
                Error r -> putStrLn $ r
            return fsm

addStateToWork :: FSM -> [String] -> IO (FSM)
addStateToWork fsm args = let fsmName = args !! 0
                              state1  = args !! 1
    in
    case ((checkForExistingFSM fsm fsmName), (checkForExistingStates fsm fsmName state1)) of
        (False, _) -> do
            putStrLn $ "FSM {" ++ fsmName ++ "} not found."
            return fsm
        (True, True) -> do
            putStrLn $ "State {" ++ state1 ++ "} already exists!."
            return fsm
        (True, False) -> 
            let newState = addStateTo fsm fsmName state1 in
                do putStrLn $ "Ok! - " ++ state1
                   return newState   

addIStateToWork :: FSM -> [String] -> IO (FSM)
addIStateToWork fsm args = let fsmName = args !! 0
                               state1  = args !! 1
    in
    case ((checkForExistingFSM fsm fsmName), (checkForExistingStates fsm fsmName state1)) of
        (False, _) -> do
            putStrLn $ "FSM {" ++ fsmName ++ "} not found."
            return fsm
        (True, False) -> do
            putStrLn $ "Not a valid state for FSM {" ++ fsmName ++ "}"
            return fsm
        (True, True) -> 
            let (str, newState) = addIStateTo fsm fsmName state1 in
                do putStrLn str
                   return newState

addFStateToWork :: FSM -> [String] -> IO (FSM)
addFStateToWork fsm args = let fsmName = args !! 0
                               state1  = args !! 1
    in
    case ((checkForExistingFSM fsm fsmName), (checkForExistingStates fsm fsmName state1)) of
        (False, _) -> do
            putStrLn $ "FSM {" ++ fsmName ++ "} not found."
            return fsm
        (True, False) -> do
            putStrLn $ "Not a valid state for FSM {" ++ fsmName ++ "}"
            return fsm
        (True, True) -> 
            let (str, newState) = addFStateTo fsm fsmName state1 in
                do putStrLn str
                   return newState

addTransToWork :: FSM -> [String] -> IO (FSM)
addTransToWork fsm args = let fsmName = args !! 0
                              state1  = args !! 1
                              state2  = args !! 2
                              value   = args !! 3 
    in
    case ((checkForExistingFSM fsm fsmName), ((checkForExistingStates fsm fsmName state1) && (checkForExistingStates fsm fsmName state2))) of
        (False, _) -> do 
            putStrLn $ "FSM {" ++ (args !! 0) ++ "} not found."
            return fsm
        (True, False) -> do
            putStrLn $ "Not a valid pair of states for the FSM {" ++ (args !! 0) ++ "}"
            return fsm
        (True,True) -> do
            let newState = addTransTo fsm fsmName state1 state2 value
            putStrLn $ "OK!, new trans: (" ++ state1 ++ state2 ++ value ++ ")."
            return newState

createFsmWork :: FSM -> [String] -> IO (FSM)
createFsmWork fsm args = do
    fsmName <- try (return (args !! 0)) :: IO (Either SomeException String)
    case fsmName of
        Left ex -> do
            missingArgsFunc ex
            return fsm
        Right fname ->
            let s' = addFsmByName fname fsm in
                case s' of
                    Just newState -> do
                        putStr "Ok! Name: "
                        putStrLn $ name (newState !! 0)
                        return newState
                    Nothing -> do
                        putStrLn "Invalid Name. Try again."
                        return fsm

determineWork :: FSM -> [String] -> IO (FSM)
determineWork fsm args = do
    let fsmName = args !! 0
    case (checkForExistingFSM fsm fsmName) of
        False -> do 
            putStrLn $ "FSM {" ++ (args !! 0) ++ "} not found."
            return fsm
        True -> do
            newState <- determineWorkAux (getFsmByName fsm fsmName)
            putStrLn "OK!, new fsm:"
            putStrLn $ ppFsm [newState]
            return (replaceFSM fsm newState)

loadFileWork :: FSM -> [String] -> IO (FSM)
loadFileWork fsm args = do
    fsmcode <- try (readFile $ args !! 0) :: IO (Either SomeException String)
    case fsmcode of -- checking for existing file
        Left ex -> do
            missingArgsFunc ex
            return fsm
        Right content -> do
            case parseComm content of -- checking for parser
                Ok m -> do 
                        workAround m fsm
                Error r -> do
                           putStrLn $ r
                           return fsm   

workAround :: Comm -> FSM -> IO (FSM)
workAround (VarDef var vt value) fsm = 
    case vt of
        VTMachine -> createFsmWork fsm [value]
        VTWord -> return fsm -- TODO!!
workAround (Assign var val) fsm = return fsm -- TODO!!
workAround (Apply fsmf var (L [])) fsm = return fsm -- TODO !!
workAround (Apply fsmf var (L (x:xs))) fsm = do
    faux <- workAround (Apply2 fsmf var x) fsm
    workAround (Apply fsmf var (L (xs))) faux
workAround (Apply2 fsmf var value) fsm = 
    case fsmf of
        AddS  -> addStateToWork fsm [var,value]
        SIS   -> addIStateToWork fsm [var,value]
        SFS   -> addFStateToWork fsm [var,value]
        _     -> do 
            putStrLn "ERROR"
            return fsm
workAround (Apply3 _ var (TL [])) fsm = 
    return fsm
workAround (Apply3 nimp var (TL (x:xs))) fsm = do
    faux <- addTransToWork fsm (fromTuple var x)
    workAround (Apply3 nimp var (TL (xs))) faux
workAround (Seq c1 c2) fsm = do
    nfsm <- workAround c1 fsm
    workAround c2 nfsm


fromTuple :: String -> (String, String, String) -> [String]
fromTuple fsmname (st1, st2, val) = [fsmname, st1, st2, val]

-- workAround (Seq c1 c2) = do
--     workAround c1
--     workAround c2

                -- aux funcs --   
--------------------------------------------------
--------------------------------------------------