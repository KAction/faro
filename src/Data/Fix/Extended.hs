module Data.Fix.Extended
  ( module M,
    para,
  )
where

import Data.Fix (Fix (..), foldFix)
import qualified Data.Fix as M

-- | A variant of cata (a.k.a foldFix) in which recursive positions also
-- include the original sub-tree, in addition to the result of folding that
-- sub-tree.

--  This functionality is provided out-out-box by "recursion-schemes" package,
--  but unfortunately, hnix expression is implemented in terms of "Fix" from
--  "data-fix" package, so it seems easier to port several missing functions
--  than coerce back and forth between two versions of "Fix".

para :: (Functor f) => (f (Fix f, a) -> a) -> Fix f -> a
para f e = snd $ foldFix g e
  where
    g x = (Fix $ fmap fst x, f x)
