{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Lib
  ( entry
  , eval
  , initEnv
  , tokenize
  , parseTokens
  ) where

import qualified Data.Map as M
import           Data.Map   (Map)
import           System.IO  (hFlush, stdout)
import           Text.Read  (readMaybe, Lexeme (Number))
import           Data.Char  (isSpace)
import           Data.List  (dropWhileEnd)

import Types      (Value(..), Token(..), Stack, Env, ParserError(..), ProgramError(..))
import           Data.Either (Either(..))
import GHC.Num (Num(fromInteger))

-- | Entry point: start REPL with an initial stack
entry :: Stack -> IO ()
entry = repl initEnv

initEnv :: Env
initEnv = M.empty

-- | REPL loop
repl :: Env -> Stack -> IO ()
repl env stk = do
  putStr "bprog> "
  hFlush stdout
  line <- getLine
  case tokenize line >>= parseTokens of
    Left err -> putStrLn (show err)  >> repl env stk
    Right ts ->
      case eval env stk ts of
        Left err     -> putStrLn ("Error: " ++ show err) >> repl env stk
        Right (e,s') -> print s' >> repl e s'

tokenize :: String -> Either ParserError [String]
tokenize = go [] . dropWhile isSpace
  where
    go acc [] = Right (reverse acc)

    -- 1) Start of a string: either \" or "
    go acc ('\\':'"':cs) = parseLit acc cs
    go acc ('"':cs)      = parseLit acc cs

    -- 2) Anything else
    go acc cs =
      let (tok, rest) = break isSpace cs
      in go (tok:acc) (dropWhile isSpace rest)

    -- Helper to pull out up to the next " and handle a trailing backslash
    parseLit acc cs =
      case break (=='"') cs of
        (raw, '"':rest) ->
          let -- if the raw content ends in a backslash, strip it off
              raw' = if not (null raw) && last raw == '\\'
                       then init raw
                       else raw
              -- now drop leading spaces inside the literal
              body = dropWhile isSpace raw'
              lit  = '"' : body ++ "\""
          in go (lit : acc) (dropWhile isSpace rest)
        _ -> Left IncompleteString


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Parse raw tokens into AST Tokens
parseTokens :: [String] -> Either ParserError [Token]
parseTokens = go [] where
  go acc [] = Right (reverse acc)
  go acc ("[":ts) = do
    (inner, rest) <- collect "[" "]" ts
    toks <- go [] inner
    go (TLit (VList (map tokenToValueOrSym toks)) : acc) rest
  go acc ("{":ts) = do
    (inner, rest) <- collect "{" "}" ts
    syms <- go [] inner
    go (TLit (VQuote syms) : acc) rest
  go acc (t:ts)
    | Just i <- readMaybe t :: Maybe Integer = go (TLit (VInt i):acc) ts
    | Just f <- readMaybe t :: Maybe Double  = go (TLit (VFloat f):acc) ts
    | t == "True"  = go (TLit (VBool True):acc) ts
    | t == "False" = go (TLit (VBool False):acc) ts
    | isString t     = let raw = read t :: String in go (TLit (VString (trim raw)):acc) ts
    | otherwise      = go (TSym t:acc) ts
  isString s = length s >= 2 && head s == '"' && last s == '"'

  -- This function tries to get a Value from TLit, otherwise preserves the symbol
  tokenToValueOrSym :: Token -> Value
  tokenToValueOrSym (TLit v) = v
  tokenToValueOrSym (TSym s) = VString s  -- or handle as needed for your eval logic

collect :: String -> String -> [String] -> Either ParserError ([String],[String])
collect open close = go 0 [] where
  go _ _ [] = Left IncompleteList
  go 0 acc (t:ts)
    | t == close = Right (reverse acc, ts)
  go lvl acc (t:ts)
    | t == open  = go (lvl+1) (t:acc) ts
    | t == close = let nl = lvl - 1 in if nl < 0 then Left IncompleteList else go nl (t:acc) ts
    | otherwise  = go lvl (t:acc) ts

-- | Main evaluator
eval :: Env -> Stack -> [Token] -> Either ProgramError (Env,Stack)
eval env (VBool c:stk) (TSym "if" : thenTok : elseTok : ts) =
  let branch = if c then thenTok else elseTok in
  case branch of
    TLit (VQuote body) ->
      case eval env stk body of
        Left err         -> Left err
        Right (env',stk')-> eval env' stk' ts
    TLit v -> eval env (v:stk) ts
    TSym s -> eval env stk (TSym s : ts)
    _ -> Left ExpectedQuotation

eval env stk (TSym "map" : TSym s : ts)
  | M.member s env =
      eval env stk (TSym "map" : TLit (VQuote [TSym s]) : ts)
eval env stk [] = Right (env, stk)
eval env stk (TSym "times" : TSym s : ts)
  | M.member s builtinsMap = eval env stk (TSym "times" : TLit (VQuote [TSym s]) : ts)
eval env stk (TSym "times" : TSym s : ts)
  | M.member s builtinsMap = eval env stk (TLit (VQuote [TSym s]) : TSym "times" : ts)
eval env stk (t:ts) = do
  (env', stk') <- consumeToken env stk t
  let fixEager e s = do
        (e', s') <- applyEager e s
        if e' == e && s' == s then Right (e, s) else fixEager e' s'
  (env'', stk'') <- fixEager env' stk'
  eval env'' stk'' ts


consumeToken :: Env -> Stack -> Token -> Either ProgramError (Env,Stack)
consumeToken env stk = \case
  TLit v -> Right (env, v:stk)
  TSym s -> Right (env, VString s : stk)

-- | Loop until `check` yields True, return final value or error
loopList :: Env -> [Token] -> [Token] -> Value -> Either ProgramError Value
loopList env inc check v0 = go v0 where
  go v = case eval env [v] check of
    Left err                -> Left err
    Right (_, VBool True:_) -> Right v
    Right (_, VBool False:_) -> case eval env [v] inc of
      Left err        -> Left err
      Right (_, v':_) -> go v'
    _ -> Left ExpectedBool

-- | Eager pattern matching, returns new env/stack or error
applyEager :: Env -> Stack -> Either ProgramError (Env, Stack)
applyEager env stk = case stk of
  VList xs : rest
    | let xs' = map (\v -> case v of
                              VString s | Just v' <- M.lookup s env -> v'
                              other                                  -> other
                          ) xs
    , xs' /= xs
    -> Right (env, VList xs' : rest)
  -- variable lookup
  VString name : rest
    | Just (VQuote body) <- M.lookup name env ->
        case eval env rest body of
          Left err      -> Left err
          Right (e',s') -> Right (e', s')
  VString name : rest
    | Just v <- M.lookup name env -> Right (env, v : rest)

  -- list‐based loops
  VQuote inc : VQuote check : VString "loop" : start@(VList _) : rest ->
    case loopList env inc check start of
      Left err    -> Left err
      Right final -> Right (env, final : rest)

  VQuote check : VQuote inc : VString "loop" : start@(VList _) : rest ->
    case loopList env inc check start of
      Left err    -> Left err
      Right final -> Right (env, final : rest)

  -- scalar loops (inc-then-check)
  VQuote inc : VQuote check : VString "loop" : start : rest ->
    runScalar start [] inc check rest

  -- scalar loops (check-then-inc)
  VQuote check : VQuote inc : VString "loop" : start : rest ->
    runScalarCheck start [] check inc rest

  -- times: negative check
  VQuote body : VString "times" : VInt n : rest
    | n < 0    -> Left ExpectedNumber
    | otherwise-> Right (goQuoteTimes n env rest body)

  -- times: repeat any value v, n times
  v : VString "times" : VInt n : rest
    | n < 0    -> Left ExpectedNumber
    | otherwise-> Right (env, replicate (fromInteger n) v ++ rest)

  -- each (quoted)
  VQuote body : VString "each" : VList xs : rest ->
    let goElem x = case eval env [x] body of
                     Right (_, y:_) -> y
                     Left _         -> VBool False
    in Right (env, map goElem (reverse xs) ++ rest)

  -- each (symbolic)
  VString fn : VString "each" : VList xs : rest
    | M.member fn builtinsMap
      || case M.lookup fn env of Just (VQuote _) -> True; _ -> False
    ->
      let
        tokens = case M.lookup fn env of
                   Just (VQuote body) -> body
                   _                  -> [TSym fn]
        goElem x = case eval env [x] tokens of
                     Right (_, y:_) -> y
                     Left _         -> VBool False
      in
        Right (env, map goElem (reverse xs) ++ rest)

  -- map
  VQuote body : VString "map" : VList xs : rest ->
    let goElem x = case eval env [x] body of
                     Right (_, y:_) -> y
                     Left _         -> VBool False
    in Right (env, VList (map goElem xs) : rest)

  -- foldl (symbolic)
  VString fn : VString "foldl" : acc : VList xs : rest
    | M.member fn builtinsMap || M.member fn env ->
        let body = [TSym fn]
            f a x = case eval env [x,a] body of
                      Right (_, y:_) -> y
                      Left _         -> a
            acc' = foldl f acc xs
        in Right (env, acc' : rest)

  -- foldl (quoted)
  VQuote body : VString "foldl" : acc : VList xs : rest ->
    let f a x = case eval env [x,a] body of
                  Right (_, y:_) -> y
                  Left _         -> a
        acc' = foldl f acc xs
    in Right (env, acc' : rest)

  -- if
  pElse : pThen : VString "if" : VBool c : rest ->
    let chosen = if c then pThen else pElse in case chosen of
      VQuote body -> case eval env rest body of Left err -> Left err; Right r -> Right r
      v           -> Right (env, v : rest)

  -- user-defined functions
  VString w : rest | Just (VQuote body) <- M.lookup w env ->
    case eval env rest body of Left err -> Left err; Right r -> Right r

  -- built-in primitives
  VString s : rest | Just op <- M.lookup s builtinsMap ->
    case op env rest of Left err -> Left err; Right r -> Right r

  -- no eager match
  _ -> Right (env, stk)
  where
    goTimes 0 e stk _ = (e, stk)
    goTimes k e stk (VQuote b) = case eval e stk b of
      Left err      -> error (show err)
      Right (e2,s2) -> goTimes (k-1) e2 s2 (VQuote b)

    goQuoteTimes 0 e stk _ = (e, stk)
    goQuoteTimes k e stk body = case eval e stk body of
      Left err      -> error (show err)
      Right (e2,s2) -> goQuoteTimes (k-1) e2 s2 body

    runScalar v acc inc check rest = case eval env [v] check of
      Left err -> Left err
      Right (_, VBool True : _)  -> Right (env, (v : acc) ++ rest)
      Right (_, VBool False : _) -> case eval env [v] inc of
        Left err       -> Left err
        Right (_,v':_) -> runScalar v' (v:acc) inc check rest
      _ -> Left ExpectedBool

    runScalarCheck v acc check inc rest = case eval env [v] check of
      Left err -> Left err
      Right (_, VBool True : _)  -> Right (env, (v : acc) ++ rest)
      Right (_, VBool False : _) -> case eval env [v] inc of
        Left err       -> Left err
        Right (_,v':_) -> runScalarCheck v' (v:acc) check inc rest
      _ -> Left ExpectedBool




-- | Primitive type
type Prim = Env -> Stack -> Either ProgramError (Env,Stack)

data Builtin = VBuiltin Prim

-- | Built-in list
builtins :: [(String,Builtin)]
builtins =
  [ ("dup",   VBuiltin primDup)
  , ("swap",  VBuiltin primSwap)
  , ("pop",   VBuiltin primPop)
  , ("+",     VBuiltin (binNumOp (+)  (+)))
  , ("-",     VBuiltin (binNumOp (-)  (-)))
  , ("*",     VBuiltin (binNumOp (*)  (*)))
  , ("/",     VBuiltin primFDiv)
  , ("div",   VBuiltin primIDiv)
  , ("==",    VBuiltin primEq)
  , ("<",     VBuiltin primLt)
  , (">",     VBuiltin primGt)
  , ("&&",    VBuiltin primAnd)
  , ("||",    VBuiltin primOr)
  , ("not",   VBuiltin primNot)
  , ("head", VBuiltin primHead)
  , ("tail", VBuiltin primTail)
  , ("cons", VBuiltin primCons)
  , ("append", VBuiltin primAppend)
  , (":=", VBuiltin primAssign)
  , ("fun", VBuiltin primFunction)
  , ("exec", VBuiltin primExec)
  , ("parseInteger", VBuiltin primParseInteger)
  , ("parseFloat", VBuiltin primParseFloat)
  , ("length", VBuiltin primLength)
  , ("\\", VBuiltin primSkip)
  , ("empty", VBuiltin primEmpty)
  , ("words", VBuiltin primWords)
  , ("%", VBuiltin primMod)
  ]

builtinsMap :: Map String Prim
builtinsMap = M.fromList [(n,p) | (n,VBuiltin p)<-builtins]

-- | Stack primitives
primDup,primSwap,primPop::Prim
primDup  e stk =  case stk of
  x:xs -> Right(e,x:x:xs)
  _  -> Left StackEmpty
primSwap e stk = case stk of
  x:y:xs -> Right(e,y:x:xs)
  _ -> Left ProgramFinishedWithMultipleValues
primPop e stk = case stk of
  _:xs -> Right(e,xs)
  _ -> Left StackEmpty

-- | Numeric ops
primFDiv,primIDiv::Prim

primFDiv= binFloatOp (/)
primIDiv= binIntOp   div

binNumOp iop fop env stk = case stk of
  -- both Ints => Int
  VInt   r : VInt   l : xs ->
    Right (env, VInt   (iop l r)              : xs)

  -- Int (left) then Float (right) => Float
  VFloat r : VInt   l : xs ->
    Right (env, VFloat (fop (fromInteger l) r) : xs)

  -- Float (left) then Int (right) => Float
  VInt   r : VFloat l : xs ->
    Right (env, VFloat (fop l (fromInteger r)) : xs)

  -- both Floats => Float
  VFloat r : VFloat l : xs ->
    Right (env, VFloat (fop l r)              : xs)

  _ -> Left ExpectedNumber
  
binIntOp f e stk = case stk of
  VInt b    : VInt a    : xs -> Right (e, VInt (f a b)                   : xs)
  VFloat b  : VInt a    : xs -> Right (e, VInt (f a (round b))           : xs)
  VInt b    : VFloat a  : xs -> Right (e, VInt (f (round a) b)           : xs)
  VFloat b  : VFloat a  : xs -> Right (e, VInt (f (round a) (round b))   : xs)
  _                          -> Left ExpectedNumber

binFloatOp f e stk = case stk of
  VFloat a:VFloat b:xs   -> Right (e, VFloat (f b a)                        : xs)
  VInt   bi:VFloat a:xs  -> Right (e, VFloat (f (fromInteger bi) a)         : xs)
  VFloat a:VInt   bi:xs  -> Right (e, VFloat (f a (fromInteger bi))         : xs)
  VInt   bi:VInt   ai:xs -> Right (e, VFloat (f (fromInteger ai) (fromInteger bi))                        : xs)
  _                      -> Left ExpectedNumber


-- | Comparison
primLt,primGt::Prim
primLt = cmpOp (<)
primGt = cmpOp (>)
cmpOp :: (Double -> Double -> Bool) -> Prim
cmpOp f env stk = case stk of
  VInt b   : VInt a   : xs ->
    Right (env, VBool (f (fromInteger a) (fromInteger b)) : xs)

  VFloat b : VFloat a : xs ->
    Right (env, VBool (f a b) : xs)

  VFloat b : VInt a   : xs ->
    Right (env, VBool (f (fromInteger a) b) : xs)

  VInt b   : VFloat a : xs ->
    Right (env, VBool (f a (fromInteger b)) : xs)

  _ ->
    Left ExpectedEnumerable



-- | Equality
primEq :: Prim
primEq e stk = case stk of
  -- Int vs Int
  VInt a   : VInt b   : xs -> Right (e, VBool (a == b)            : xs)
  -- Float vs Float
  VFloat a : VFloat b : xs -> Right (e, VBool (a == b)            : xs)
  -- Int vs Float
  VInt a   : VFloat b : xs -> Right (e, VBool (fromInteger a == b): xs)
  -- Float vs Int
  VFloat a : VInt b   : xs -> Right (e, VBool (a == fromInteger b): xs)
  -- fallback to structural equality (e.g. strings, lists, bools, quotes…)
  x         : y       : xs -> Right (e, VBool (x == y)            : xs)
  _                         -> Left ExpectedEnumerable

-- | Boolean
primAnd,primOr::Prim
primAnd=boolOp(&&)
primOr =boolOp(||)
boolOp f e stk = case stk of
  VBool b:VBool a:xs -> Right(e,VBool(f a b):xs)
  _ -> Left ExpectedBoolOrNumber
primNot::Prim
primNot e stk = case stk of
  VBool b:xs -> Right(e,VBool(not b):xs)
  VInt  i:xs -> Right(e,VInt(negate i):xs)
  _ -> Left ExpectedBool


primHead::Prim
primHead e stk = case stk of
  VList (x:xs):ys -> Right(e,x:ys)
  _ -> Left ExpectedList

primTail::Prim
primTail e stk = case stk of
  VList (x:xs):ys -> Right(e,VList xs:ys)
  _ -> Left ExpectedList

primCons :: Prim
primCons e stk = case stk of
  VList xs : y : zs -> Right (e, VList (y : xs) : zs)
  _ -> Left ExpectedList

primAppend :: Prim
primAppend e stk = case stk of
  VList xs : VList ys : zs -> Right (e, VList (ys ++ xs) : zs)
  _ -> Left ExpectedList

primAssign :: Prim
primAssign e stk = case stk of
  v : VString s : xs -> Right (M.insert s v e, xs)
  _ -> Left FailedVariable

primFunction :: Prim
primFunction e stk = case stk of
  VQuote body : VString s : xs -> Right (M.insert s (VQuote body) e, xs)
  _ -> Left FailedFunction

primTimes :: Prim
primTimes e stk = Right (e, VString "times" : stk)

-- | Execurre a VQuote block
primExec :: Prim
primExec e stk = case stk of
  VQuote body : xs -> eval e xs body
  _ -> Left ExpectedQuotation

primParseInteger, primParseFloat::Prim
primParseInteger e stk = case stk of
  VString s:xs -> case readMaybe s :: Maybe Integer of
    Just i  -> Right (e, VInt i : xs)
    Nothing -> Left NumberConversionError
  _ -> Left ExpectedString
primParseFloat e stk = case stk of
  VString s:xs -> case readMaybe s :: Maybe Double of
    Just f  -> Right (e, VFloat f : xs)
    Nothing -> Left NumberConversionError
  _ -> Left ExpectedString

primLength::Prim
primLength e stk = case stk of
  VList xs:ys -> Right (e, VInt (fromIntegral (length xs)) : ys)
  VQuote xs:ys -> Right (e, VInt (fromIntegral (length xs)) : ys)
  VString s:ys -> Right (e, VInt (fromIntegral (length s)) : ys)
  _ -> Left ExpectedList

primSkip::Prim
primSkip e stk = Right (e, stk)

primEmpty::Prim
primEmpty e stk = case stk of
  VList xs:ys -> Right (e, VBool (null xs) : ys)
  VQuote xs:ys -> Right (e, VBool (null xs) : ys)
  _ -> Left ExpectedList

primWords::Prim
primWords e stk = case stk of
  VString s:ys -> Right (e, VList (map VString (words s)) : ys)
  _ -> Left ExpectedVariable

primMod::Prim
primMod e stk = case stk of
  VInt a:VInt b:xs -> Right (e, VInt (a `mod` b) : xs)
  VFloat a:VInt b:xs -> Right (e, VInt (round a `mod` b) : xs)
  VInt a:VFloat b:xs -> Right (e, VInt (a `mod` round b) : xs)
  VFloat a:VFloat b:xs -> Right (e, VInt (round a `mod` round b) : xs)
  _ -> Left NumberConversionError