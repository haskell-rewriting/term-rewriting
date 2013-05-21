-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Christian Sternagel

module Data.Rewriting.Context.Ops (
    apply,
    compose,
    ofTerm,
) where

import Control.Monad
import Data.Rewriting.Pos
import Data.Rewriting.Term.Type
import Data.Rewriting.Context.Type

-- | Apply a context to a term (i.e., replace the hole in the context by the
-- term).
apply :: Ctxt f v -> Term f v -> Term f v
apply Hole t = t
apply (Ctxt f ts1 ctxt ts2) t = Fun f (ts1 ++ apply ctxt t : ts2)

-- | Compose two contexts (i.e., replace the hole in the left context by the
-- right context).
compose :: Ctxt f v -> Ctxt f v -> Ctxt f v
compose Hole c2 = c2
compose (Ctxt f ts1 c1 ts2) c2 = Ctxt f ts1 (c1 `compose` c2) ts2

-- | Create a context from a term by placing the hole at a specific position.
ofTerm :: Term f v -> Pos -> Maybe (Ctxt f v)
ofTerm _ [] = Just Hole
ofTerm (Fun f ts) (i:p) = do
    guard (i >= 0 && i < length ts)
    let (ts1, t:ts2) = splitAt i ts
    ctxt <- ofTerm t p
    return (Ctxt f ts1 ctxt ts2)
ofTerm _ _ = Nothing
