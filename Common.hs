module Common where

type FWord = String
type Variable = String

data VT = VTMachine 
        | VTWord 
        deriving Show

--data Fsm = (alph, st, is, fs, trs)
--           deriving Show

data FSMFunc = SAlph                    
              | AddS                    
              | SIS                    
              | SFS                    
              | STS                    
              | None                  
              deriving Show

data Lists  = L [String]               
            deriving Show

data Comm   = VarDef Variable VT Variable  -- Definición de variable
            | Assign FWord Variable        -- Asignación en variable
            | Seq Comm Comm                -- Secuencia de comandos ejecutables
            | Apply FSMFunc Variable Lists -- Aplicacion
            deriving Show
