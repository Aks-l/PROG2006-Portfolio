module Types
  ( Value(..)
  , Token(..)
  , Stack
  , Env
  ) where

import qualified Data.Map as M

-- | The core values in BPROG
data Value
  = VInt    Integer
  | VFloat  Double
  | VBool   Bool
  | VString String
  | VList   [Value]
  | VQuote  [Token]
  deriving (Eq, Show)

-- | Tokens (after parsing but before evaluation)
data Token
  = TLit Value
  | TSym String
  deriving (Eq, Show)

-- | The operand stack
type Stack = [Value]

-- | The symbol environment
type Env = M.Map String Value
