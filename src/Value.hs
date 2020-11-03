module Value where

import Name
import Term

import Data.Maybe (fromJust)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap

type Scope = HashMap Name Value -- evaluated value

initScope :: Scope
initScope = HMap.empty

data Value
  = VInt Int
  | VBool Bool
  | VClosure Name Term Scope
  | VString String
  -- ^ this is why we need the Value datatype. To encode the closure value.
  deriving (Show)

eval :: Scope -> Term -> Value
eval _ (Lit (LInt int))    = VInt int
eval _ (Lit (LBool bool))  = VBool bool
eval _ (Lit (LString str)) = VString str
eval s (Var name)          = fromJust (HMap.lookup name s)
eval s (Lam arg _ body)    = VClosure arg body s
eval s (App term1 term2)   = apply (eval s term1) (eval s term2)
eval s (BinOps op x y)     = binOps op (eval s x) (eval s y)

apply :: Value -> Value -> Value
apply (VClosure name term scope) val = eval (HMap.insert name val scope) term
apply _ _ = error "Tried to apply non-closure"

binOps :: PrimOp -> Value -> Value -> Value
binOps Add (VInt x) (VInt y) = VInt (x + y)
binOps Sub (VInt x) (VInt y) = VInt (x - y)
binOps Mul (VInt x) (VInt y) = VInt (x * y)
binOps Div (VInt x) (VInt y) = VInt (x `quot` y)
binOps _   _        _        = error "Not a number"
