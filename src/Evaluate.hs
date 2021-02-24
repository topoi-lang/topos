{-# LANGUAGE Strict, BlockArguments, ScopedTypeVariables #-}
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

import Prelude hiding (span)
import qualified Data.ByteString as B
import qualified Syntax
import Data.Maybe
import Data.Name
import FlatParse (Span, Result(Err, OK))
import qualified FlatParse as FP
import Data.List (findIndex)
import Data.HashMap.Strict (HashMap, insert, empty)


import qualified Lexer
import Parser

data Ctx = Ctx
  { _fileName :: FilePath
  , _src      :: B.ByteString
  } deriving Show

span2Name :: Ctx -> Span -> Name
span2Name ctx s = Name $ FP.unsafeSpan2ByteString (_src ctx) s

data Spine = SNil | SApp Spine ~Val
data Val = VLam Name (Val -> Val) | VLoc Name Spine | VTop Name Spine ~Val

type TopEnv = HashMap Name Term -- Unordered, so Name doesn't need Ord instance
type LocEnv = [(Name, Int)]

findFirst :: LocEnv -> Name -> Maybe Int
findFirst loc n = findIndex (\(name, _) -> name == n) loc

-- also a kind of locally nameless
data Term
  = Local Int -- De Bruijn Index, corresponding to Bounded
  | Top   Name -- top level name, corresponding to Free Var
  | App Term Term
  | Lam Name Term
  deriving Show

toTerm :: Ctx -> TopEnv -> LocEnv -> Syntax.Expr -> Term
toTerm ctx top loc = \case

  Syntax.Var span -> let name = span2Name ctx span in
    case findFirst loc name of
      Just dbi -> Local dbi
      Nothing  -> Top name

  Syntax.App fn arg -> App (toTerm ctx top loc fn) (toTerm ctx top loc arg)

  -- \x . e = [x/e]
  Syntax.Lam span _t e -> Lam name (toTerm ctx top shiftedLoc e)
    where
      name = span2Name ctx span
      shiftedLoc = (name, 0): fmap (\(n, i) -> (n, i+1)) loc

  -- let x: a = t in u
  -- [x/t] u
  Syntax.Let _ span _a t u ->
    App (Lam name (toTerm ctx top shiftedLoc t)) (toTerm ctx top loc u)
      where
        name = span2Name ctx span
        shiftedLoc = (name, 0): fmap (\(n, i) -> (n, i+1)) loc

  Syntax.T{} -> undefined 
  Syntax.F{} -> undefined
  Syntax.Zero{} -> undefined
  Syntax.Succ{} -> undefined
  Syntax.Pred{} -> undefined
  Syntax.IsZero{} -> undefined

testTerm :: B.ByteString -> Term
testTerm bs = case Lexer.runParser pSrc (_src ctx) of
  OK ee _ _ -> toTerm ctx empty mempty ee
  _ -> error "some error"
  where
    ctx = Ctx{ _fileName="", _src=bs }
