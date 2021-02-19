{-# LANGUAGE Strict, BlockArguments #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Evaluate where

{-
    Reduction of expressions

    Q: What is strict and non-strict evaluation?
    A: An evaluation strategy is strict if the arguments to a lambda expression
       are necessarily evaluated before a lambda is reduced. A language in
       which the arguments are not necessarily evaluated before a lambda is
       reduced is non-strict.

    Q: What is call by value (CBV) strategy?
    A: Most languges use a call by value strategy, in which only outermost
       redexes are reduces and where a redex is reduces only when its right
       hand side has already been reduces to a value- a term that is finished
       computing and cannot be reduced any further.

       id (id (\z. id z))
       -> id (\z. id z)
       -> \z. id z
       [terminate]
    
    Q: What is call by name (CBN) strategy?
    A: TODO

    Q: What is call by push value (CBPV)?
    A: It is a fine-grained calculus of effects, subsuming call-by-value and
       call-by-name. It is very precise about what gets evaluated when and can
       express both call-by-value and call-by-name.

    NOTE: The result doesn't depend on the evaluation order if it is pure.
    Reference: http://plzoo.andrej.com/language/levy.html
-}

import qualified Data.ByteString as B
import Syntax
import Data.Maybe

type Name = B.ByteString

data Spine = SNil | SApp Spine ~Value

data Value
  = VLam Name (Value -> Value)
  | VLocal Name Spine
  | VTop Name Spine ~Value

type TopEnv = [(Name, Value)]
newtype LocalEnv = LocalEnv [(Name, Value)]

fresh :: LocalEnv -> Name -> Name
fresh (LocalEnv r) x = case lookup x r of
  Nothing -> x
  Just{}  -> fresh (LocalEnv r) (x <> "'")

-- | `vapp` acts on both branches of VTop.
-- The `vapp t u` can be arbitrarily costly, so it is delayed
vapp :: Value -> Value -> Value
vapp (VLam _ t)    ~u = t u
vapp (VLocal x sp) ~u = VLocal x (SApp sp u)
vapp (VTop x sp t) ~u = VTop x (SApp sp u) (vapp t u)

eval :: TopEnv -> LocalEnv -> Expr -> Value
eval top local = undefined
