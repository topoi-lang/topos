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
    Reference: https://www.reddit.com/r/haskell/comments/4rvssi/is_nameless_representation_worth_the_trouble_why/
-}

import qualified Data.ByteString as B
import Syntax
import Data.Maybe
import Data.Name
import FlatParse (Span)
import qualified FlatParse as FP

data Ctx = Ctx
  { _fileName :: FilePath
  , _src      :: B.ByteString
  } deriving Show

span2Name :: Ctx -> Span -> Name
span2Name ctx s = Name $ FP.unsafeSpan2ByteString (_src ctx) s

data Spine = SNil | SApp Spine ~Val
data Val = VLam Name (Val -> Val) | VLoc Name Spine | VTop Name Spine ~Val

type TopEnv = [(Name, Val)]
type LocEnv = [(Name, Val)]

data Var a = Free a | Bound Int
type LocallyNameless a = Var a

eval :: Ctx -> TopEnv -> LocEnv -> Expr -> Val
eval ctx top loc = \case
  Var s   -> VTop x SNil (fromJust $ lookup x top) where x = span2Name ctx s
  App t u -> vapp (eval ctx top loc t) (eval ctx top loc u)
  Let _ s _t t u -> eval ctx top ((x, eval ctx top loc t):loc) u where x = span2Name ctx s
  Lam s _t e -> VLam x $ \u -> eval ctx top ((x, u):loc) e where x = span2Name ctx s
  -- T
  -- F ?

vapp :: Val -> Val -> Val
vapp (VLam _ t)    ~u = t u
vapp (VLoc x sp)   ~u = VLoc x (SApp sp u)
vapp (VTop x sp t) ~u = VTop x (SApp sp u) (vapp t u)