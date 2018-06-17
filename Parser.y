{
module Parser where

import Failable

import Common
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parse_Comm Comm

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='     			 { TEquals }
    '"'                  { TQuotation }
    ';'                  { TSemiColon }
    ','                  { TComma }
    '['                  { TBOpen }
    ']'                  { TBClose }
    '{'                  { TBrOpen }
    '}'                  { TBrClose }
    '('                  { TPrOpen }
    ')'                  { TPrClose }
    VARDEF               { TVarDef }
    VAR                  { TVar $$ }
    FWORD                { TVTWord }
    MACHINE              { TMachDef }
    SETALPHABET          { TSetAlph }
    ADDSTATES            { TAddStates }
    SETINITIALSTATE      { TSetInitState }
    SETFINALSTATES       { TSetFStates}
    SETTRANSITIONS       { TSetTrans }

%right ';'
%%

Comm    : Comm ';' Comm                         { Seq $1 $3 }
        | VARDEF VAR '=' ValueExp               { VarDef $2 VTWord $4 }
        | MACHINE VAR '=' ValueExp              { VarDef $2 VTMachine $4 }
        | VAR '=' ValueExp                      { Assign $1 $3 }
        | Functions '(' VAR ',' List ')'        { Apply $1 $3 $5 }

ValueExp    :  '"' VAR '"'                      { $2 }

Functions   : SETALPHABET                       { SAlph }
            | ADDSTATES                         { AddS }
            | SETINITIALSTATE                   { SIS }
            | SETFINALSTATES                    { SFS }
            | SETTRANSITIONS                    { STS }

List        : '[' List_Char ']'                 { L $2 }

List_Char   : ValueExp ',' List_Char         { $1 : $3 }
            | ValueExp                       { [$1] }

{

type LineNumber = Int
type P a = String -> LineNumber -> Failable a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Error e -> Error e

returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Error err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Error e -> k e s l

happyError :: P a
happyError = \ _ i -> Error $ "Error de parseo -> Linea " ++ (show (i::LineNumber))

data Token  = TEquals
            | TComma
            | TQuotation
            | TSemiColon
            | TBOpen
            | TBClose
            | TBrOpen
            | TBrClose
            | TPrOpen
            | TPrClose
            | TVarDef
            | TVar String
            | TVTWord
            | TVTMachine
            | TSetAlph
            | TAddStates
            | TSetInitState
            | TSetFStates
            | TSetTrans
            | TMachDef
            | TEOF
            deriving Show

----------------------------------

lexer cont s = case s of
                   [] -> cont TEOF []
                   ('\n':s)  ->  \line -> lexer cont s (line + 1)
                   (c:cs)
                         | isSpace c -> lexer cont cs
                         | isAlphaNum c -> lexAlphaNum (c:cs)
                   -- Comentarios
                   ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                   ('{':('-':cs)) -> consumirBK 0 0 cont cs
                   ('-':('}':cs)) -> \ line -> Error $ "Parse error: Line " ++ (show line) ++ " -> Unclosed commetary"
                   -- Simbolos
                   ('=':cs) -> cont TEquals cs
                   (',':cs) -> cont TComma cs
                   ('"':cs) -> cont TQuotation cs
                   (';':cs) -> cont TSemiColon cs
                   ('[':cs) -> cont TBOpen cs
                   (']':cs) -> cont TBClose cs
                   ('{':cs) -> cont TBrOpen cs
                   ('}':cs) -> cont TBrClose cs
                   ('(':cs) -> cont TPrOpen cs
                   (')':cs) -> cont TPrClose cs
                   unknown -> \line ->
                                       Error $ "Parse error: Line " ++ (show line)
                                       ++ "-> Unrecognized "
                                       ++ (show $ take 10 unknown) ++ "..."
                   where lexAlphaNum cs@(x:_) = case span isAlphaNum cs of
                                                        ("let", rest)                   -> cont TVarDef rest
                                                        ("mac", rest)                   -> cont TVTMachine rest
                                                        ("str", rest)                   -> cont TVTWord rest
                                                        ("Machine", rest)               -> cont TMachDef rest
                                                        ("setAlphabet", rest)           -> cont TSetAlph rest
                                                        ("addStates", rest)             -> cont TAddStates rest
                                                        ("setInitialState", rest)       -> cont TSetInitState rest
                                                        ("setFinalStates", rest)        -> cont TSetFStates rest
                                                        ("setTransitions", rest)        -> cont TSetTrans rest
                                                        (var,rest)                      -> cont (TVar var) rest
                         consumirBK anidado cl cont s = case s of
                                                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                                                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs
                                                              ('-':('}':cs)) -> case anidado of
                                                                                 0 -> \line -> lexer cont cs (line+cl)
                                                                                 _ -> consumirBK (anidado-1) cl cont cs
                                                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                                                              (_:cs) -> consumirBK anidado cl cont cs

parseComm s = parse_Comm s 1

}
