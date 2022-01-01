{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Faro.Check.Expr.NameAndVersion (check, xcheck, XWarning) where

import Data.Fix.Extended (para, unFix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid (First (First), getFirst)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Faro.Orphans ()
import Faro.Types (Warning (..))
import Nix.Expr.Types
  ( Binding (NamedVar),
    NBinaryOp (NApp),
    NKeyName (StaticKey),
  )
import Nix.Expr.Types.Annotated
  ( NExprLoc,
    NExprLocF,
    SourcePos,
    spanBegin,
    pattern NBinary_,
    pattern NSelect_,
    pattern NSet_,
    pattern NSym_,
  )
import Prettyprinter (Pretty (pretty), hardline)

data Acc = Acc
  { warnings :: Set XWarning,
    pending :: Maybe (SourcePos, SourcePos) -- (name, version)
  }

pattern AttrL a = StaticKey a :| []

pattern SKey name = StaticKey name :| []

data XWarning = XWarning
  { function :: Text,
    functionPos :: SourcePos,
    namePos :: SourcePos,
    versionPos :: SourcePos
  }
  deriving (Eq, Ord)

posOf :: Text -> [Binding NExprLoc] -> Maybe SourcePos
posOf name =
  let match = \case
        NamedVar (SKey name') _ pos | name' == name -> Just pos
        _ -> Nothing
   in getFirst . foldMap (First . match)

xcheck :: NExprLoc -> Set XWarning
xcheck x = warnings $ para fn x
  where
    fn :: NExprLocF (NExprLoc, Acc) -> Acc
    fn expr =
      let w :: Set XWarning
          w = foldMap (warnings . snd) expr
       in case expr of
            NSet_ _ _ binders -> Acc w pending
              where
                binders'1 :: [Binding NExprLoc]
                binders'1 = map (fmap fst) binders

                namePos, versionPos :: Maybe SourcePos
                namePos = posOf "name" binders'1
                versionPos = posOf "version" binders'1

                pending = (,) <$> namePos <*> versionPos
            NBinary_ _ NApp (f, af) (e, ae) -> Acc w' Nothing
              where
                w' :: Set XWarning
                w' = maybe id Set.insert wnew w

                funcNames :: Set Text
                funcNames = Set.fromList ["mkDerivation"]

                wnew :: Maybe XWarning
                wnew = do
                  (npos, vpos) <- pending ae
                  case unFix f of
                    NSym_ fpos name
                      | name `Set.member` funcNames ->
                        pure $
                          XWarning
                            name
                            (spanBegin fpos)
                            npos
                            vpos
                    NSelect_ fpos _ (AttrL name) _
                      | name `Set.member` funcNames ->
                        pure $
                          XWarning
                            name
                            (spanBegin fpos)
                            npos
                            vpos
                    _ -> Nothing
            _ -> Acc w Nothing

warning :: XWarning -> Warning
warning xw =
  Warning
    { name = "name-and-version",
      subject =
        pretty (function xw)
          <> " called with both `name' and `version' attributes",
      details =
        hardline
          <> "Function "
          <> pretty (function xw)
          <> " is called at "
          <> pretty (functionPos xw)
          <> "\n"
          <> "Attribute `name' is defined at "
          <> pretty (namePos xw)
          <> "\n"
          <> "Attribute `version' is defined at "
          <> pretty (versionPos xw)
          <> "\n",
      explanation = mempty
    }

check :: NExprLoc -> [Warning]
check = map warning . Set.toList . xcheck
