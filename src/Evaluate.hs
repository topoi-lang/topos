{-# LANGUAGE Strict, BlockArguments #-}

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

import Prelude hiding (span, lookup)
import qualified Data.ByteString as B
import qualified Syntax
import Data.Maybe
import Data.Name
import FlatParse (Span, Result(Err, OK))
import qualified FlatParse as FP
import Data.List (findIndex, elemIndex)
import Data.HashMap.Strict (HashMap, insert, empty, lookup)
import qualified Data.HashMap.Strict as HM

import qualified Lexer
import Parser

data Ctx = Ctx
  { _fileName :: FilePath
  , _src      :: B.ByteString
  } deriving Show

span2Name :: Ctx -> Span -> Name
span2Name ctx s = Name $ FP.unsafeSpan2ByteString (_src ctx) s

type TopEnv = [(Name, Term)]
type LocEnv = [Name] -- doesn't need to shift `(name, 0): fmap (\(n, i) -> (n, i+1)) loc`

findFirst :: LocEnv -> Name -> Maybe Int
findFirst = flip elemIndex

-- also a kind of locally nameless
data Term
  = Local Int -- De Bruijn Index, corresponding to Bounded
  | Top   Name -- top level name, corresponding to Free Var
  | App Term Term
  | Lam Name Term
  | Bottom Bool
  deriving Show

toTerm :: Ctx -> TopEnv -> LocEnv -> Syntax.Expr -> Term
toTerm ctx top loc = \case

  Syntax.Var span -> let name = span2Name ctx span in
    case findFirst loc name of
      Just dbi -> Local dbi
      Nothing  -> Top name

  Syntax.App fn arg -> App (toTerm ctx top loc fn) (toTerm ctx top loc arg)

  -- \x . e = [x/e]
  Syntax.Lam _ span _t e -> Lam name (toTerm ctx top (name:loc) e)
    where
      name = span2Name ctx span

  -- let x: a = t in u
  -- [x/t] u
  Syntax.Let _ span _a t u ->
    App (Lam name (toTerm ctx top (name:loc) u)) (toTerm ctx top loc t)
      where
        name = span2Name ctx span

  Syntax.T{} -> Bottom True
  Syntax.F{} -> Bottom False
  Syntax.Zero{} -> undefined
  Syntax.Succ{} -> undefined
  Syntax.Pred{} -> undefined
  Syntax.IsZero{} -> undefined

testTerm :: B.ByteString -> Term
testTerm bs = case Lexer.runParser pSrc (_src ctx) of
  OK ee _ _ -> toTerm ctx [] mempty ee
  _ -> error "some error"
  where
    ctx = Ctx{ _fileName="", _src=bs }

-- TODO: should write some tests regarding to the term equality
  -- NOTE: https://boarders.github.io/posts/locally-nameless.html

-------------------------------------------------------------------------------
-- Original: https://gist.github.com/AndrasKovacs/a0e0938113b193d6b9c1c0620d853784
-- More Clearer: https://github.com/Boarders/NbE-untyped/blob/master/Main.hs

data Spine
  = SNil              -- empty spine for top level definitions
  | SApp Spine ~Val   -- stuck application
  deriving Show

data Val
  = VLam Name (Val -> Val)
  | VLoc Int  Spine
  | VTop Name Spine ~Val
  | VBottom Bool
  deriving Show

-- TODO: try a way to get rid of this.
instance Show (Val -> Val) where
  show _ = "[ C L O S U R E ]"

type VTopEnv = HashMap Name Val
type VLocEnv = [Val] -- TODO: check whether this is needed or not

evalLocEnv :: Int -> VLocEnv -> Val
evalLocEnv = flip (!!)

evalTopEnv :: Name -> VTopEnv -> Val
evalTopEnv name = fromJust . lookup name

eval :: VTopEnv -> VLocEnv -> Term -> Val
eval top loc = \case
  Local i -> evalLocEnv i loc
  Top n   -> evalTopEnv n top
  App t u -> vapp (eval top loc t) (eval top loc u)
  Lam n t -> VLam n \val -> eval top (val:loc) t
  Bottom bool -> VBottom bool

vapp :: Val -> Val -> Val
vapp (VLam _n body)        ~arg = body arg
vapp (VLoc lvl spine)      ~arg = VLoc lvl  (SApp spine arg)
vapp (VTop name spine val) ~arg = VTop name (SApp spine arg) (vapp val arg)
vapp (VBottom _)           _ = error "applied non function in vapp"

evalWithTerm :: TopEnv -> Term -> Val
evalWithTerm top tm = eval topvals [] tm where
  topvals = foldl
    (\topmap (name, term) -> insert name (eval topmap [] term) topmap)
    empty
    top

--- Test

($$) :: Term -> Term -> Term
($$) = App
infixl 8 $$

predefinedTop :: TopEnv
predefinedTop =
  [ (Name "zero", Lam (Name "s") . Lam (Name "z") $ Local 0)
  , (Name "succ",
       let
         nsz = Local 2 $$ Local 1 $$ Local 0
         s   = Local 1
       in
         Lam (Name "n") . Lam (Name "s") . Lam (Name "z") $ s $$ nsz)
  ]

-- evalWithTerm predefinedTop (testTerm "succ zero")