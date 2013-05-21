-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

{-# LANGUAGE StandaloneDeriving #-}

-- selected samples for tests

module Samples where

import Data.Rewriting.Term (Term (..))
import qualified Data.Rewriting.Term as Term
import Data.Rewriting.Rule (Rule (..))
import Data.Rewriting.Pos (Pos)
import Data.Rewriting.CriticalPair (CP (..))
import Data.Rewriting.Substitution (GSubst (..))
import qualified Data.Rewriting.Substitution as Subst
import qualified Data.Rewriting.Context as Ctxt

a = Fun 'a' []
b = Fun 'b' []
c = Fun 'c' []

f = Fun 'f'
g = Fun 'g'
h = Fun 'h'

x = Var 'x'
y = Var 'y'
z = Var 'z'

(-->) = Rule

trs1 = [f[x, x] --> g[a, x], f[a, y] --> h[a], a --> b]
cps1 = [mkCP r1 r2 [], mkCP r3 r2 [0]]
   where [r1, r2, r3] = trs1

mkCP :: (Eq f, Ord v) => Rule f v -> Rule f v -> Pos -> CP f v v
mkCP lRule rRule pos = let
    (llhs, lrhs) = (Term.map id Left (lhs lRule), Term.map id Left (rhs lRule))
    (rlhs, rrhs) = (Term.map id Right (lhs rRule), Term.map id Right (rhs rRule))
    Just subst = Subst.unify llhs (rlhs !!! pos)
    Just rlhs' = Ctxt.ofTerm rlhs pos
  in
    CP{ top = subst `Subst.apply` rlhs, right = subst `Subst.apply` rrhs,
        left = rlhs' `Ctxt.apply` (subst `Subst.apply` lrhs),
        leftRule = lRule, rightRule = rRule, leftPos = pos, subst = subst }

deriving instance (Show f, Show v, Show v') => Show (CP f v v')

(!!!) :: Term f v -> Pos -> Term f v
t !!! [] = t
Fun _ ts !!! (p:ps) = (ts !! p) !!! ps
