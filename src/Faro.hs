{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Faro (main) where

import Control.Monad (forM_)
import Data.Fix.Extended (Fix (..), foldFix, para)
import Data.Foldable (Foldable (fold))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (First (First), getFirst)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Faro.Check.Expr.NameAndVersion (check)
import Faro.Types (Warning, details, subject)
import Nix.Expr.Types
  ( Binding (NamedVar),
    NBinaryOp (NApp),
    NExpr,
    NExprF (NBinary, NSelect, NSym),
    NKeyName (StaticKey),
  )
import Nix.Expr.Types.Annotated
  ( NExprLoc,
    NExprLocF,
    SourcePos,
    sourceColumn,
    sourceLine,
    sourceName,
    spanBegin,
    unPos,
    pattern NBinary_,
    pattern NSelect_,
    pattern NSet_,
    pattern NSym_,
  )
import Nix.Parser (parseNixFile, parseNixFileLoc)
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    strOption,
    (<**>),
  )
import Prettyprinter (Doc, Pretty (pretty), hardline, nest)
import Prettyprinter.Util (putDocW)
import System.Exit (exitFailure)

newtype CommandArguments = CommandArguments {nixFile :: FilePath}
  deriving (Show)

commandArgumentsP :: Parser CommandArguments
commandArgumentsP =
  CommandArguments
    <$> strOption
      ( mempty
          <> long "nix-file"
          <> metavar "FILE"
          <> help "Nix source file"
      )

commandArgumentsIO :: IO CommandArguments
commandArgumentsIO = execParser $ info (commandArgumentsP <**> helper) mod
  where
    mod =
      mempty
        <> fullDesc
        <> header "faro -- linter for Nix packages"

main :: IO ()
main = do
  CommandArguments {nixFile} <- commandArgumentsIO
  expr <-
    parseNixFileLoc nixFile >>= \case
      Left err -> print err >> exitFailure
      Right expr -> pure expr
  forM_ (check expr) $ \w -> putDocW 80 (subject w <> nest 4 (details w))
