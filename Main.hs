{-# OPTIONS -XRecordWildCards #-}

module Main where

import System.IO
import System.IO.Error
import Data.List
import Data.Char
import Data.Ord
import System.Console.Haskeline
import Failable 

import Text.PrettyPrint.HughesPJ (render)

import Common
import Parser
import PrettyPrinter
data FSM = S String

main :: IO ()
main = do main2 (S "aa")

main2 :: FSM -> IO ()
main2 s@(S x)= 
        do 
            putStr $ "> "
            line <- getLine
            case line of 
                "q" -> return ()
                _ -> do
                    sara <- readFile "programon.fsm"
                    case parseComm sara of
                        Ok m -> do 
                                 res <- handleComm m s--putStrLn $ render $ (ppComm m)
                                 main2 res
                        Error r -> putStrLn $ r
                    let s' = updateSbyLine line s in
                        return main s'
            


updateSbyLine :: String -> FSM -> FSM
updateSbyLine _ s = s


handleComm :: Comm -> FSM -> IO FSM
handleComm comm s@(S x) = do 
                            case comm of
                                VarDef var vt value -> putStrLn $ var
                                Assign var val -> putStrLn $ var
                                Seq c1 c2 -> handleComm c1 s
                                Apply fsmf var (L list) -> putStrLn $ var 
                                Apply2 fsmf var var1 -> putStrLn $ var
                                Apply3 fsmf var (TL tlist) -> putStrLn $ var
                            return s

--import Failable
--import Eval
--import PrettyPrinter

-- Módulo principal para ejecutar el intérprete

--------------------------------------------------------------------------------

-- Representa un estado del intérprete
--data State = S { files :: [String],
--                 env :: Env,
--                 working :: Bool
--                } deriving Show

-- Estado Vacío
--emptyState :: State
--emptyState = S ["programon.fsm"] [] True

--------------------------------------------------------------------------------}
{-
-- Limpia limpia la pantalla
clearScr :: IO ()
clearScr = do putStrLn "\ESC[H\ESC[J"
              putStrLn "Finally, a FSM interpreter, yay!."

-- Código de colores
blackColor       = 30
redColor         = 31
greenColor       = 32
yellowColor      = 33
blueColor        = 34
magentaColor     = 35
cyanColor        = 36
whiteColor       = 37

-- Imprime un texto con un color
printWColor :: String -> Int -> InputT IO ()
printWColor x color = outputStrLn $ "\x1b[" ++ show color ++ "m" ++ x ++ "\x1b[0m"

-- Imprime un error
printError :: String -> InputT IO ()
printError x = printWColor x redColor

-- Imprime un mensaje verde
printOk :: String -> InputT IO ()
printOk x = printWColor x greenColor

-- Función principal
main :: IO ()
main = do clearScr
          s <- reload emptyState
          runInputT defaultSettings (mainLoop s)

-- Bucle principal del intérprete
mainLoop :: State -> InputT IO ()
mainLoop s@(S{..}) = do 
                        minput <- getInputLine "> "
                        case minput of
                            Nothing -> return ()
                            Just "" -> mainLoop s
                            Just input -> do 
                                            c <- parseCommand input
                                            case c of 
                                                Just command -> 
                                                    do ns <- handleCommand command s
                                                       mainLoop ns
                                                _ -> do ns <- runStringSecure input s
                                                        mainLoop ns 

{-
mainLoop s@(S{..}) = if not working then return () else
                     do  maybeLine <- getInputLine "> "
                         case maybeLine of
                             Nothing -> do putStrLn ""
                                           return () -- EOF / control-d
                             Just "" -> mainLoop s
                             Just line -> do putHistory line
                                             c <- parseCommand line
                                             case c of
                                                 Just command -> do ns <- handleCommand command s
                                                                    mainLoop ns
                                                 _ -> do ns <- runStringSecure line s
                                                         mainLoop ns
-}
--------------------------------------------------------------------------------
-- Comandos

-- Representa un comando
data Command    = Load String
                | Reload
                | Disassociate String
                | Print Variable
                | Reset
                | Help
                | ClearScreen
                | Exit
                | Null String
                deriving Show

-- Lee un comando y retorna un comando opcional
parseCommand :: String -> InputT IO (Maybe Command)
parseCommand str =  if null str then return $ Just (Help) else
                    case splitCommand str of
                        Nothing -> return Nothing
                        Just (com, rest) -> case (map toLower com) of
                                                "load"         -> return $ Just (Load rest)
                                                "l"            -> return $ Just (Load rest)
                                                "reload"       -> return $ Just (Reload)
                                                "r"            -> return $ Just (Reload)
                                                "disassociate" -> return $ Just (Disassociate rest)
                                                "d"            -> return $ Just (Disassociate rest)
                                                "print"        -> return $ Just (Print rest)
                                                "p"            -> return $ Just (Print rest)
                                                "reset"        -> return $ Just (Reset)
                                                "c"            -> return $ Just (Reset)
                                                "help"         -> return $ Just (Help)
                                                "h"            -> return $ Just (Help)
                                                "?"            -> return $ Just (Help)
                                                "clearscreen"  -> return $ Just (ClearScreen)
                                                "cls"          -> return $ Just (ClearScreen)
                                                "exit"         -> return $ Just (Exit)
                                                "quit"         -> return $ Just (Exit)
                                                "q"            -> return $ Just (Exit)
                                                _              -> return $ Just (Null com)

-- Maneja un comando:
-- Toma el comando a trabajar y el estado actual
handleCommand :: Command -> State -> InputT IO State
handleCommand comm s@(S {..}) = do  case comm of
                                        Load path -> addFile path s
                                        Reload ->   reload s
                                        Disassociate f -> do printOk $ "Se ha desasociado el archivo \"" ++ f ++ "\""
                                                             return s { files =  delete f files }
                                        Print var -> printVar var s
                                        Reset -> do putStrLn "Reiniciando entorno"
                                                    reload emptyState
                                        Help -> do printHelp
                                                   return s
                                        ClearScreen -> do clearScr
                                                          return s
                                        Exit -> do return $ s {working = False}
                                        Null com -> do  printError $ "Error: El comando \"" ++ com ++ "\" no existe."
                                                        return s

--------------------------------------------------------------------------------

-- Recarga el sistema
reload :: State -> InputT IO State
reload s@(S {..}) = loadFilesSecure files s


-- Carga una lista de achivos de manera segura
-- Esto es se carga todo o nada
loadFilesSecure :: [String] -> State -> InputT IO State
loadFilesSecure files s = let ns = s {env = []} in
                          do temp <- loadFiles files ns ns
                             case temp of
                               Ok state -> return state
                               Error err -> do  printError err
                                                return s

-- Carga una lista de archivos en un estado (Puede fallar, esto debe ser capturado)
loadFiles :: [String] -> State -> State -> InputT IO (Failable State)
loadFiles [] s _ = do printOk "Carga completa"
                      return (Ok s)
loadFiles (x:xs) s original = do temp <- loadFile x s
                                 case temp of
                                    Ok ns -> loadFiles xs ns original
                                    Error err -> return (Error err)

-- Carga un archivo en un estado
loadFile :: String -> State -> InputT IO (Failable State)
loadFile path s = do c <- tryIOError (readFile path)
                     case c of
                         Left _ -> return (Error $ "No se pudo leer el archivo \"" ++ path ++ "\".")
                         Right content -> do putStrLn $ "Cargando \"" ++ path ++ "\"."
                                             temp <- runString content s
                                             case temp of
                                                 Ok ns -> return (Ok ns)
                                                 Error err -> return (Error err)

-- Evalua un String y retorna un estado que o bien contiene el resultado de toda la
-- ejecución o el estado original si hubo algún fallo
runStringSecure :: String -> State -> InputT IO State
runStringSecure content s = do  temp <- runString content s
                                case temp of
                                    Ok ns -> return ns
                                    Error err ->  do printError err
                                                     return s

-- Evalua un String de FRL. Retorna un entorno que puede fallar
runString :: String -> State -> IO (Failable State)
runString content s@(S{..}) = case parseComm content of
                                Ok comm -> case eval' comm env of
                                                Ok ns ->     return (Ok $ s {env = ns})
                                                Error err -> return (Error err)
                                Error err -> return (Error err)

-- Agrega un archivo a la lista de archvos si este no está presenta
addFile :: String -> State -> InputT IO State
addFile path s@(S {..}) = if elem path files
                          then do printError $ "El archivo \"" ++ path ++ "\" ya estaba agregado."
                                  return s
                          else let ns = s {files = files ++ [path]} in
                          do  printOk $ "El archivo \"" ++ path ++ "\" se há agregado."
                              loadFilesSecure [path] ns
                              return ns

-- Toma el nombre de una variable y un estado e intenta imprimir su contenido
printVar :: String -> State -> IO State
printVar [] s@(S {..}) = do putStrLn $ render $ ppEnv env
                            return s
printVar var s@(S{..}) = do case lookup var env of
                                Just a -> putStrLn $ render $ ppVarType a
                                _ -> printError $ "No se ha encontrado la variable \"" ++ var ++ "\""
                            return s

-- Imprime el mensaje de ayuda al usuario
printHelp :: IO ()
printHelp = let helps =   [ (":l | :load", "Carga y asocia un archivo a la sesión actual.") ,
                            (":r | :reload","Recarga todos los archivos (Se perderán las variables ingresadas en el intérprete)"),
                            (":d | :disassociate", "Quita de la lista de archivos a cargar el archivo ingresado (No se recargan las variables)"),
                            (":p | :print", "Muestra el contenido de una variable, o el entorno si no se ingresa el nombre de la variable"),
                            (":c | :reset", "Reinicia el intérprete"),
                            (":h | :? | :help", "Imprime la ayuda"),
                            (":cls | :clearscreen", "Limpia el contenido de la pantalla"),
                            (":q | :exit", "Sale del intérprete") ]
                len = 2 + (length $ maximumBy (comparing length) $ map fst helps)
                list = map (\(x, y) -> " " ++ x ++ (generateWhite $ len - (length x)) ++ y) helps
            in do mapM_ putStrLn list
                    where  generateWhite :: Int -> String
                           generateWhite 0 = ""
                           generateWhite n = " " ++ (generateWhite $ n - 1)


splitCommand :: String -> Maybe (String, String)
splitCommand (':':xs) = let (a, b) = span (/= ' ') xs in
                        case b of
                            (_ : c) -> Just (a, c)
                            _ -> Just (a, "")
splitCommand _ = Nothing
-}