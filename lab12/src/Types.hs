module Types
  ( Value(..)
  , Token(..)
  , Stack
  , Env
  , ProgramError(..)
  , ParserError(..)
  ) where

import qualified Data.Map as M
import Data.List (intercalate)

-- | The core values in BPROG
data Value
  = VInt    Integer
  | VFloat  Double
  | VBool   Bool
  | VString String
  | VList   [Value]
  | VQuote  [Token]
  deriving (Eq)

instance Show Value where
  show (VInt i)      = show i
  show (VFloat d)    = show d
  show (VBool True)  = "True"
  show (VBool False) = "False"
  show (VString s)   = show s
  show (VList vs)    = "[" ++ intercalate "," (map show vs) ++ "]"
  show (VQuote ts)   = "{ " ++ unwords (map show ts) ++ " }"

-- | Tokens (after parsing but before evaluation)
data Token
  = TLit Value
  | TSym String
  deriving (Eq)

instance Show Token where
  show (TSym s) = s
  show (TLit v) = show v

-- | The operand stack
type Stack = [Value]

-- | The symbol environment
type Env = M.Map String Value

-- | Represents program execution errors.
data ProgramError =
     StackEmpty
   | UnknownSymbol
   | ExpectedBool
   | ExpectedBoolOrNumber
   | ExpectedEnumerable
   | ExpectedQuotation
   | ExpectedList
   | ExpectedVariable
   | DivisionByZero
   | ProgramFinishedWithMultipleValues
   | NumberConversionError
     deriving (Eq, Show)

-- | Represents parser errors.
data ParserError =
    IncompleteString
  | IncompleteList
  | IncompleteQuotation
