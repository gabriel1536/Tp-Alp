module Eval where

import Common
import Failable
import RLF

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

--------------------------------------------------------------------------------
-- Entornos y Variables

-- Entornos
type EnvItem = (FWord, VarType)  -- Elemento de Entorno
type Env = [EnvItem]                -- Entorno

-- Entorno nulo
initState :: Env
initState = []

-- Tipos de variables
data VarType = VarMachine Fsm
             | Var FWord
             deriving Show

--------------------------------------------------------------------------------
-- Estados

-- Monada estado
newtype StateError a = StateError { runStateError :: Env -> Failable (a, Env) }

instance Monad StateError where
    return a = StateError (\s -> Ok (a,s))
    m >>= f  = StateError (\s -> do (x, s') <- runStateError m s
                                    runStateError (f x) s')

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure   = return
  (<*>)  = ap


-- Clase para representar monadas con estado de variables
class Monad m => MonadState m where
  -- Busca el valor de una variable
  lookfor :: FWord -> m VarType
  -- Cambia el valor de una variable
  update :: FWord -> VarType -> m ()

instance MonadState StateError where
    lookfor v = StateError (\s -> lookfor' v s s)
                            where lookfor' v ((a, b):ss) es | v == a = Ok (b, es)
                                                            | v /= a = lookfor' v ss es
                                  lookfor' v _ _            = Error $ "Uncaught ReferenceError:\"" ++ v ++ "\" is not defined"

    update v i = StateError (\s -> update' v i s [])
                          where   update' v i [] temp = Ok ((), temp ++ [(v, i)])
                                  update' v i ((a, b):ss) temp | v == a = case (b, i) of
                                                                              (VarMachine _, VarMachine _) -> Ok ((), temp ++ ((v, i):ss))
                                                                              (Var _, Var _) -> Ok ((), temp ++ ((v, i):ss))
                                                                              (_, _) -> Error ("Variable update error. Types do not match: \"" ++ v ++ "\"")
                                                              | otherwise = update' v i ss (temp ++ [(a,b)])

-- Clase para representar monadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: String -> m a

instance MonadError StateError where
    throw err = StateError (\_ -> Error err)

--------------------------------------------------------------------------------
-- Evaluadores globales
{-
-- Evalúa un AST en el estado nulo
eval :: Comm -> Failable Env
eval comm = eval' comm initState

-- Evalúa un AST en un estado pasado como argumento
eval' :: Comm -> Env -> Failable Env
eval' comm env = case runStateError (evalCommand comm) env of
                    Ok (_, e) -> Ok e
                    Error err -> Error err

--------------------------------------------------------------------------------
-- Evaluadores internos

-- Evalúa un comando
evalCommand :: (MonadError m, MonadState m) => Comm -> m ()
evalCommand (VarDef var VTMachine value) = do  val <- evalValue value
                                            case val of
                                                VTMachine f -> update var val
                                                _ -> throw "Type Word does not match type Machine"
evalCommand (VarDef var VTWord value) = do  val <- evalValue value
                                            case val of
                                                VTWord f -> update var val
                                                _ -> throw "Type Machine does not match type Word"
evalCommand (Assign var value) = do val <- evalValue value
                                    update var val
evalCommand (Seq c1 c2) = do evalCommand c1
                             evalCommand c2

-- Evalúa una lista (El resultado es una lista de enteros solamente)
evalList :: (MonadError m, MonadState m) => Lists -> m [Int]
evalList (L l) = return l
evalList (Cat ls1 ls2 ) = do l1 <- evalList ls1
                             l2 <- evalList ls2
                             return (l1 ++ l2)
evalList (LVar label) = do  ls <- lookfor label
                            case ls of
                                VarList l -> return l
                                _ -> throw $ "La variable \"" ++ label ++ "\" no es una lista."

-- Evalúa una función sobre una lista
evalRLFunc :: (MonadError m, MonadState m) => RLFunc -> [Int] -> m [Int]
evalRLFunc Ol list = return $ ol list
evalRLFunc Or list = return $ RLF.or list
evalRLFunc Rl list = if hasMinimumSize 1 list   then return $ rl list
                                                else throw "No se puede aplicar Rl sobre una lista vacía"
evalRLFunc Rr list = if hasMinimumSize 1 list   then return $ rr list
                                                else throw "No se puede aplicar Rr sobre una lista vacía"
evalRLFunc Sl list = if hasMinimumSize 1 list   then return $ sl list
                                                else throw "No se puede aplicar Sl sobre una lista vacía"
evalRLFunc Sr list = if hasMinimumSize 1 list   then return $ sr list
                                                else throw "No se puede aplicar Sr sobre una lista vacía"
evalRLFunc r@(Rep f) list = if hasMinimumSize 2 list
                            then if needsIterate list then do   l <- evalRLFunc f list
                                                                evalRLFunc r l
                                                      else return list
                            else throw "Para aplicar una repetición se requieren al menos dos elelemntos"
evalRLFunc (FVar var) list = do  item <- lookfor var
                                 case item of
                                    VarFunc f -> evalRLFunc f list
                                    _ -> throw "No se puede evaluar una lista en una lista"
evalRLFunc (Comp f1 f2) list = do   l <- evalRLFunc f1 list
                                    evalRLFunc f2 l
evalRLFunc (None) list = return list

-- Evalúa un valor
evalValue :: (MonadError m, MonadState m) => Value -> m VarType
evalValue (Func f) = return $ VarFunc f
evalValue (List ls) = do l <- evalList ls
                         return $ VarList l
evalValue (Apply f ls) = do l <- evalList ls
                            ev <- evalRLFunc f l
                            return $ VarList ev
-}