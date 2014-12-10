-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

module Data.Rewriting.CriticalPair.Ops (
    -- * pairs of rewrite systems
    cps,
    cpsIn,
    cpsOut,
    -- * single rewrite systems
    cps',
    cpsIn',
    cpsOut',
) where

import Data.Rewriting.CriticalPair.Type
import Data.Rewriting.Substitution
import Data.Rewriting.Rule.Type
import qualified Data.Rewriting.Term as Term
import Data.Rewriting.Pos
import Data.Rewriting.Rules.Rewrite (listContexts)

import Data.Maybe
import Control.Monad
import Data.List

-- cpW does all the hard work:
-- Given a function that returns contexts to consider for rewriting,
-- unify the contexts of the right rule's lhs with the left rule's lhs,
-- and return the resulting critical pairs.
cpW :: (Ord v, Ord v', Eq f)
    => (Term f (Either v v')
        -> [(Pos, Context f (Either v v'), Term f (Either v v'))])
    -> Rule f v -> Rule f v' -> [(CP f v v')]
cpW f rl rr = do
    let rl' = Term.map id Left (lhs rl)
        rr' = Term.map id Right (lhs rr)
    (pos, ctx, rr'') <- f rr'
    guard $ not (Term.isVar rr'')
    subst <- maybeToList $ unify rl' rr''
    return CP{
        left = apply subst (ctx (Term.map id Left (rhs rl))),
        top = apply subst rr',
        right = apply subst (Term.map id Right (rhs rr)),
        leftRule = rl,
        leftPos = pos,
        rightRule = rr,
        subst = subst
    }


-- TODO: find a better place for this kind of contexts.
type Context f v = Term f v -> Term f v

-- Calculate contexts of a term, in pre-order.
-- In particular, the root context is returned first.
contexts :: Term f v -> [(Pos, Context f v, Term f v)]
contexts t@(Var _)    = [([], id, t)]
contexts t@(Fun f ts) = ([], id, t) : do
    (i, ctxL, t) <- listContexts ts
    (pos, ctxT, t') <- contexts t
    return (i : pos, Fun f . ctxL . ctxT, t')

-- Determine critical pairs for a pair of rules.
cp :: (Ord v, Ord v', Eq f)
    => Rule f v -> Rule f v' -> [(CP f v v')]
cp  = cpW contexts

-- Determine outer critical pairs for a pair of rules.
cpOut :: (Ord v, Ord v', Eq f)
    => Rule f v -> Rule f v' -> [(CP f v v')]
cpOut = cpW (take 1 . contexts)

-- Determine inner critical pairs for a pair of rules.
cpIn :: (Ord v, Ord v', Eq f)
    => Rule f v -> Rule f v' -> [(CP f v v')]
cpIn = cpW (tail . contexts)


-- | Determine all critical pairs for a pair of TRSs.
cps :: (Ord v, Ord v', Eq f) => [Rule f v] -> [Rule f v']
    -> [(CP f v v')]
cps  trs1 trs2 = do
    rl <- trs1
    rr <- trs2
    cp rl rr

-- | Determine all inner critical pairs for a pair of TRSs.
--
-- A critical pair is /inner/ if the left rewrite step is not a root step.
cpsIn :: (Ord v, Ord v', Eq f) => [Rule f v] -> [Rule f v']
    -> [(CP f v v')]
cpsIn trs1 trs2 = do
    rl <- trs1
    rr <- trs2
    cpIn rl rr

-- | Determine outer critical pairs for a pair of TRSs.
--
-- A critical pair is /outer/ if the left rewrite step is a root step.
cpsOut :: (Ord v, Ord v', Eq f) => [Rule f v] -> [Rule f v']
    -> [(CP f v v')]
cpsOut trs1 trs2 = do
    rl <- trs1
    rr <- trs2
    cpOut rl rr


-- | Determine all critical pairs of a single TRS with itself.
--
-- Unlike @cps@, @cps'@ takes symmetries into account. See 'cpsIn'' and
-- 'cpsOut'' for details.
cps' :: (Ord v, Eq f) => [Rule f v] -> [(CP f v v)]
cps'  trs = cpsIn' trs ++ cpsOut' trs

-- | Determine all inner critical pairs of a single TRS with itself.
--
-- The result of @cpsIn' trs@ differs from @cpsIn trs trs@ in that overlaps
-- of a rule with itself are returned once, not twice.
cpsIn' :: (Ord v, Eq f) => [Rule f v] -> [(CP f v v)]
cpsIn' trs = do
    r1 : trs' <- tails trs
    cpIn r1 r1 ++ do
        r2 <- trs'
        cpIn r1 r2 ++ cpIn r2 r1

-- | Determine all outer critical pairs of a single TRS with itself.
--
-- The result of @cpsOut' trs@ differs from @cpsOut trs trs@ in two aspects:
--
--  * The trivial overlaps of rules with themselves are omitted.
--
--  * Symmetry is taken into account: Overlaps between distinct rules are
--    returned once instead of twice.
cpsOut' :: (Ord v, Eq f) => [Rule f v] -> [(CP f v v)]
cpsOut' trs = do
    rl : trs' <- tails trs
    rr <- trs'
    cpOut rl rr
