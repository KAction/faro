{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Faro (main) where

import Control.Monad (forM_)
import Faro.Check.Expr.NameAndVersion (check)
import Faro.Types (details, subject)
import Nix.Parser (parseNixFileLoc)
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
import Prettyprinter (nest)
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
