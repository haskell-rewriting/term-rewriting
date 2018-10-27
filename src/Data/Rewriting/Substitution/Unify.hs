-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

{-# LANGUAGE FlexibleContexts #-}

module Data.Rewriting.Substitution.Unify (
    unify,
    unifyRef,
) where

import Data.Rewriting.Substitution.Type
import Data.Rewriting.Substitution.Ops (apply)
import qualified Data.Rewriting.Term.Ops as Term
import qualified Data.Rewriting.Term.Type as Term
import Data.Rewriting.Term.Type (Term (..))

import qualified Data.Map as M
import qualified Control.Monad.Union as UM
import qualified Data.Union as U
import Control.Monad.State
import Control.Monad.ST
import Control.Applicative
import Control.Arrow
import Data.Array.ST
import Data.Array
import Data.Maybe
import Data.Word

-- The setup is as follows:
--
-- We have a disjoint set forest, in which every node represents some
-- subterm of our unification problem. Each node is annotated by a
-- description of the term which may refer to other nodes. So we actually
-- have a graph, and an efficient implementation for joining nodes in
-- the graph, curtesy of the union find data structure. We also maintain
-- a map of variables encountered so far to their allocated node.

type UnifyM f v a = StateT (M.Map v U.Node) (UM.UnionM (Annot f v)) a

-- Each node can either represent
-- - a variable (in which case this is the only node representing that variable)
-- - an *expanded* function application with arguments represented by nodes,
-- - or a *pending* function application with normal terms as arguments,
--   not yet represented in the disjoint set forest.

data Annot f v = VarA v | FunA f [U.Node] | FunP f [Term f v]

-- Extract function symbol and arity from (non-variable) annotation.
funari :: Annot f v -> (f, Int)
funari (FunA f ns) = (f, length ns)
funari (FunP f ts) = (f, length ts)

-- Solve a system of equations between terms that are represented by nodes.
solve :: (Eq f, Ord v) => [(U.Node, U.Node)] -> UnifyM f v Bool
solve [] = return True
solve ((t, u) : xs) = do
    (t, t') <- UM.lookup t
    (u, u') <- UM.lookup u
    -- if t == u then the nodes are already equivalent.
    if t == u then solve xs else case (t', u') of
        (VarA _, _) -> do
            -- assign term to variable
            UM.merge (\_ _ -> (u', ())) t u
            solve xs
        (_, VarA _) -> do
            -- assign term to variable
            UM.merge (\_ _ -> (t', ())) t u
            solve xs
        _ | funari t' == funari u' ->
            -- matching function applications: expand ...
            -- note: avoid `do` notation because `FunA _ ts` is a "failable"
            -- pattern and `UnionM` doesn't have a `MonadFail` instance;
            -- cf. https://wiki.haskell.org/MonadFail_Proposal
            expand t t' >>= \(FunA _ ts) ->
            expand u u' >>= \(FunA _ us) ->
            UM.merge (\t _ -> (t, ())) t u >>
            -- ... and equate the argument lists.
            solve (zip ts us ++ xs)
        _ -> do
            -- mismatch, fail.
            return False

-- Expand a node: If the node is currently a pending function application,
-- turn it into an expanded one.
-- The second argument must equal the current annotation of the node.
expand :: (Ord v) => U.Node -> Annot f v -> UnifyM f v (Annot f v)
expand n (FunP f ts) = do
    ann <- FunA f <$> mapM mkNode ts
    UM.annotate n ann
    return ann
expand n ann = return ann

-- Create a new node representing a given term.
-- Variable nodes are shared whenever possible.
-- Function applications will be pending initially.
mkNode :: (Ord v) => Term f v -> UnifyM f v U.Node
mkNode (Var v) = do
    n <- gets (M.lookup v)
    case n of
        Just n -> return n
        Nothing -> do
            n <- UM.new (VarA v)
            modify (M.insert v n)
            return n
mkNode (Fun f ts) = UM.new (FunP f ts)

-- | Unify two terms. If unification succeeds, return a most general unifier
-- of the given terms. We have the following property:
--
-- > unify t u == Just s   ==>   apply s t == apply s u
--
-- /O(n log(n))/, where /n/ is the apparent size of the arguments. Note that
-- the apparent size of the result may be exponential due to shared subterms.
unify :: (Eq f, Ord v) => Term f v -> Term f v -> Maybe (Subst f v)
unify t u = do
    let -- solve unification problem
        act = do
            t' <- mkNode t
            u' <- mkNode u
            success <- solve [(t', u')]
            return (t', success)
        (union, ((root, success), vmap)) = UM.run' $ runStateT act M.empty
        -- find the successors in the resulting graph
        succs n = case snd (U.lookup union n) of
            VarA v -> []
            FunA f ns -> ns
            FunP f ts -> do v <- Term.vars =<< ts; maybeToList (M.lookup v vmap)
    guard $ success && acyclic (U.size union) succs root
    let -- build resulting substitution
        subst = fromMap $ fmap lookupNode vmap
        -- 'terms' maps representatives to their reconstructed terms
        terms = fmap mkTerm (UM.label union)
        -- look up a node in 'terms'
        lookupNode = (terms !) . U.fromNode . fst . U.lookup union
        -- translate annotation back to term
        mkTerm (VarA v) = Var v
        mkTerm (FunA f ns) = Fun f (fmap lookupNode ns)
        mkTerm (FunP f ts) = subst `apply` Fun f ts
    return subst

-- Check whether the subgraph reachable from the given root is acyclic.
-- This is done by a depth first search, where nodes are initially colored
-- white (0), then grey (1) while their children are being visited and
-- finally black (2) after the children have been processed completely.
--
-- The subgraph is cyclic iff we encounter a grey node at some point.
--
-- O(n) plus the cost of 'succs'; 'succs' is called at most once per node.
acyclic :: Int -> (U.Node -> [U.Node]) -> U.Node -> Bool
acyclic size succs root = runST $ do
    let t :: ST s (STUArray s Int Word8)
        t = undefined
    color <- newArray (0, size-1) 0 `asTypeOf` t
    let dfs n = do
            c <- readArray color (U.fromNode n)
            case c of
                0 -> do
                    writeArray color (U.fromNode n) 1
                    flip (foldr andM) (map dfs (succs n)) $ do
                        writeArray color (U.fromNode n) 2
                        return True
                1 -> return False
                2 -> return True
    dfs root

-- monadic, logical and with short-cut evaluation
andM :: Monad m => m Bool -> m Bool -> m Bool
andM a b = do
    a' <- a
    if a' then b else return False

------------------------------------------------------------------------------
-- Reference implementation

-- | Unify two terms. This is a simple implementation for testing purposes,
-- and may be removed in future versions of this library.
unifyRef :: (Eq f, Ord v) => Term f v -> Term f v -> Maybe (Subst f v)
unifyRef t u = fromMap <$> go [(t, u)] M.empty where
   go [] subst = Just subst
   go ((t, u) : xs) subst = case (t, u) of
      (Var v, t) -> add v t xs subst
      (t, Var v) -> add v t xs subst
      (Fun f ts, Fun f' ts')
          | f /= f' || length ts /= length ts' -> Nothing
          | otherwise -> go (zip ts ts' ++ xs) subst
   add v t xs subst
       | Var v == t = go xs subst
       | occurs v t = Nothing
       | otherwise =
           let app = apply (fromMap (M.singleton v t))
           in  go (fmap (app *** app) xs) (M.insert v t (fmap app subst))
   occurs v t = v `elem` Term.vars t
