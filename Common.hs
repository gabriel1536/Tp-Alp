module Common where

type FWord = String
type Variable = String

data VT = VTMachine 
        | VTWord 
        deriving Show

type Alph = [String]
type States = [String]
type IState = [String]
type FState = [String]
type Transitions = [(String, String, String)] -- State x State x a E Alphabet

--type NameOfFsm = String
data Fsm = Fsm { name        :: Variable
               , alphabet    :: Alph
               , states      :: States
               , iState      :: String
               , fState      :: FState
               , transitions :: Transitions 
               } deriving (Show)

type FSM = [Fsm]

data FSMFunc = SAlph                    
              | AddS                    
              | SIS                    
              | SFS                    
              | STS 
              | None                  
              deriving Show

data Lists  = L States             
            deriving Show

data TLists  = TL Transitions
            deriving Show

data Comm   = VarDef Variable VT Variable  -- Definición de variable
            | Assign FWord Variable        -- Asignación en variable
            | Seq Comm Comm                -- Secuencia de comandos ejecutables
            | Apply FSMFunc Variable Lists -- Aplicacion
            | Apply2 FSMFunc Variable Variable -- Aplicacion
            | Apply3 FSMFunc Variable TLists -- Aplicacion
            | Concat Lists Variable
            deriving Show
