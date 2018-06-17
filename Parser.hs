{-# OPTIONS_GHC -w #-}
module Parser where

import Failable

import Common
import Data.Maybe
import Data.Char
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,64) ([0,2028,49152,126,4,0,16,0,8,16,0,2048,0,0,0,0,0,0,0,0,0,1024,0,49152,126,1,8192,0,256,0,32768,0,8,8192,0,0,0,32768,0,2,16384,0,0,0,32,0,0,0,1,0,2,32,0,0,32768,0,8192,0,0,0,0,8192,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse_Comm","Comm","ValueExp","Functions","List","List_Char","'='","'\"'","';'","','","'['","']'","'{'","'}'","'('","')'","VARDEF","VAR","FWORD","MACHINE","SETALPHABET","ADDSTATES","SETINITIALSTATE","SETFINALSTATES","SETTRANSITIONS","%eof"]
        bit_start = st * 28
        bit_end = (st + 1) * 28
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..27]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (19) = happyShift action_4
action_0 (20) = happyShift action_5
action_0 (22) = happyShift action_6
action_0 (23) = happyShift action_7
action_0 (24) = happyShift action_8
action_0 (25) = happyShift action_9
action_0 (26) = happyShift action_10
action_0 (27) = happyShift action_11
action_0 (4) = happyGoto action_12
action_0 (6) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (19) = happyShift action_4
action_1 (20) = happyShift action_5
action_1 (22) = happyShift action_6
action_1 (23) = happyShift action_7
action_1 (24) = happyShift action_8
action_1 (25) = happyShift action_9
action_1 (26) = happyShift action_10
action_1 (27) = happyShift action_11
action_1 (4) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (11) = happyShift action_13
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (17) = happyShift action_17
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (20) = happyShift action_16
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (9) = happyShift action_15
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (20) = happyShift action_14
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_7

action_8 _ = happyReduce_8

action_9 _ = happyReduce_9

action_10 _ = happyReduce_10

action_11 _ = happyReduce_11

action_12 (11) = happyShift action_13
action_12 (28) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (19) = happyShift action_4
action_13 (20) = happyShift action_5
action_13 (22) = happyShift action_6
action_13 (23) = happyShift action_7
action_13 (24) = happyShift action_8
action_13 (25) = happyShift action_9
action_13 (26) = happyShift action_10
action_13 (27) = happyShift action_11
action_13 (4) = happyGoto action_23
action_13 (6) = happyGoto action_3
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (9) = happyShift action_22
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (10) = happyShift action_21
action_15 (5) = happyGoto action_20
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (9) = happyShift action_19
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (20) = happyShift action_18
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (12) = happyShift action_27
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (10) = happyShift action_21
action_19 (5) = happyGoto action_26
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_4

action_21 (20) = happyShift action_25
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (10) = happyShift action_21
action_22 (5) = happyGoto action_24
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (11) = happyShift action_13
action_23 _ = happyReduce_1

action_24 _ = happyReduce_3

action_25 (10) = happyShift action_30
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_2

action_27 (13) = happyShift action_29
action_27 (7) = happyGoto action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (18) = happyShift action_33
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (10) = happyShift action_21
action_29 (5) = happyGoto action_31
action_29 (8) = happyGoto action_32
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_6

action_31 (12) = happyShift action_35
action_31 _ = happyReduce_14

action_32 (14) = happyShift action_34
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_5

action_34 _ = happyReduce_12

action_35 (10) = happyShift action_21
action_35 (5) = happyGoto action_31
action_35 (8) = happyGoto action_36
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_13

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happyReduce 4 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (VarDef happy_var_2 VTWord happy_var_4
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 4 4 happyReduction_3
happyReduction_3 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (VarDef happy_var_2 VTMachine happy_var_4
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn4
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 6 4 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Apply happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 _
	(HappyTerminal (TVar happy_var_2))
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn6
		 (SAlph
	)

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn6
		 (AddS
	)

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn6
		 (SIS
	)

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn6
		 (SFS
	)

happyReduce_11 = happySpecReduce_1  6 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn6
		 (STS
	)

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (L happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  8 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 28 28 tk (HappyState action) sts stk;
	TEquals -> cont 9;
	TQuotation -> cont 10;
	TSemiColon -> cont 11;
	TComma -> cont 12;
	TBOpen -> cont 13;
	TBClose -> cont 14;
	TBrOpen -> cont 15;
	TBrClose -> cont 16;
	TPrOpen -> cont 17;
	TPrClose -> cont 18;
	TVarDef -> cont 19;
	TVar happy_dollar_dollar -> cont 20;
	TVTWord -> cont 21;
	TMachDef -> cont 22;
	TSetAlph -> cont 23;
	TAddStates -> cont 24;
	TSetInitState -> cont 25;
	TSetFStates -> cont 26;
	TSetTrans -> cont 27;
	_ -> happyError' (tk, [])
	})

happyError_ explist 28 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 :: () => P a -> (a -> P b) -> P b
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> P a
happyError' tk = (\(tokens, explist) -> happyError) tk
parse_Comm = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "D:/GitHub/haskell-platform/build/ghc-bindist/local/lib/include/ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "F:/Users/randy/AppData/Local/Temp/ghc19564_0/ghc_2.h" #-}














































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates\\\\GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 75 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 84 "templates\\\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 147 "templates\\\\GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates\\\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates\\\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
