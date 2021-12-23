{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Faro (main) where

import Data.Foldable (Foldable(fold))
import Control.Monad (forM_)
import Data.Fix.Extended (Fix (..), foldFix, para)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (First (First), getFirst)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
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
import Prettyprinter (Doc, Pretty (pretty), nest, hardline)
import Prettyprinter.Util (putDocW)
import System.Exit (exitFailure)

data Warning = WNameAndVersion
  { function :: Text,
    functionPos :: SourcePos,
    namePos :: SourcePos,
    versionPos :: SourcePos
  }
  deriving (Show, Eq, Ord)

instance Pretty SourcePos where
  pretty pos =
    pretty (sourceName pos)
      <> ":"
      <> pretty (unPos (sourceLine pos))
      <> ":"
      <> pretty (unPos (sourceColumn pos))

instance Pretty Warning where
  pretty w =
    pretty (function w)
      <> " called with both `name' and `version' attributes"
      <> nest
        4
        ( hardline <> "Function " <> pretty (function w) <> " is called at "
            <> pretty (functionPos w)
            <> "\n"
            <> "Attribute `name' is defined at "
            <> pretty (namePos w)
            <> "\n"
            <> "Attribute `version' is defined at "
            <> pretty (versionPos w)
            <> "\n"
        )

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

pattern ESym a = Fix (NSym a)

pattern ESelect a b c = Fix (NSelect a b c)

pattern EBinary a b c = Fix (NBinary a b c)

pattern AttrL a = StaticKey a :| []

data Acc = Acc
  { warnings :: Set Warning,
    pending :: Maybe (SourcePos, SourcePos) -- (name, version)
  }
  deriving (Show)

pattern SKey name = StaticKey name :| []

posOf :: Text -> [Binding NExprLoc] -> Maybe SourcePos
posOf name =
  let match = \case
        NamedVar (SKey name') _ pos
          | name' == name -> Just pos
        _ -> Nothing
   in getFirst . foldMap (First . match)

checkNameAndVersion :: NExprLoc -> Set Warning
checkNameAndVersion x = warnings $ para fn x
  where
    fn :: NExprLocF (NExprLoc, Acc) -> Acc
    fn expr =
      let w :: Set Warning
          w = fold $ fmap (warnings . snd) expr
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
                w' :: Set Warning
                w' = maybe id Set.insert wnew w

                funcNames :: Set Text
                funcNames = Set.fromList ["mkDerivation"]

                wnew :: Maybe Warning
                wnew = do
                  (npos, vpos) <- pending ae
                  case unFix f of
                    NSym_ fpos name
                      | name `Set.member` funcNames ->
                        pure $
                          WNameAndVersion
                            name
                            (spanBegin fpos)
                            npos
                            vpos
                    NSelect_ fpos _ (AttrL name) _
                      | name `Set.member` funcNames ->
                        pure $
                          WNameAndVersion
                            name
                            (spanBegin fpos)
                            npos
                            vpos
                    _ -> Nothing
            _ -> Acc w Nothing


main :: IO ()
main = do
  CommandArguments {nixFile} <- commandArgumentsIO
  expr <-
    parseNixFileLoc nixFile >>= \case
      Left err -> print err >> exitFailure
      Right expr -> pure expr
  forM_ (checkNameAndVersion expr) (putDocW 80 . pretty)
