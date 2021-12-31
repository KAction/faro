{-# LANGUAGE OverloadedStrings #-}
module Faro.Orphans () where

import Nix.Expr.Types.Annotated
  ( SourcePos,
    sourceColumn,
    sourceLine,
    sourceName,
    unPos,
  )
import Prettyprinter (Pretty (pretty))

instance Pretty SourcePos where
  pretty pos =
    pretty (sourceName pos)
      <> ":"
      <> pretty (unPos (sourceLine pos))
      <> ":"
      <> pretty (unPos (sourceColumn pos))
