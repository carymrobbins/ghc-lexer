{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.GHC.Parser.Internal.JSON where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified ApiAnnotation
import qualified FieldLabel
import qualified Module
import BasicTypes
import BooleanFormula
import Class
import FastString
import HsSyn
import Lexer
import Outputable
import SrcLoc
import Var
import TcEvidence
import TcType

-- Lexer tokens
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''FractionalLit)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''InlineSpec)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''SourceText)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RuleMatchInfo)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ApiAnnotation.IsUnicodeSyntax)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ApiAnnotation.HasE)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Token)

-- Parser nodes
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsModule)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''GenLocated)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''IE)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''IEWrappedName)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''IEWildcard)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''FieldLabel.FieldLbl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Module.ModuleName)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsDocString)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ImportDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''StringLiteral)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsBindLR)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyClDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''InstDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DerivDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Sig)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''WarningTxt)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DefaultDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ForeignDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''WarnDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''WarnDecls)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''AnnDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RuleDecls)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''VectDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''SpliceDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RoleAnnotDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DocDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ClsInstDecl)
-- $(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''LHsSigType)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''FixitySig)
-- $(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''LHsSigWcType)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''InlinePragma)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsImplicitBndrs)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DataFamInstDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsType)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Var)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyFamInstDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DerivStrategy)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''OverlapMode)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''BooleanFormula)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''AnnProvenance)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Class)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsExpr)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsWrapper)
-- $(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TcType)

locTokenToJSON :: Located Token -> Value
locTokenToJSON (L srcSpan token) = object ["srcSpan" .= srcSpan, "token" .= token]

putJSON :: ToJSON a => a -> IO ()
putJSON = LC8.putStr . encode

putJSONLn :: ToJSON a => a -> IO ()
putJSONLn = LC8.putStrLn . encode

instance ToJSON C8.ByteString where
  toJSON bs = toJSON $ C8.unpack bs

instance FromJSON C8.ByteString where
  parseJSON = withText "ByteString" $ pure . T.encodeUtf8

instance ToJSON SDoc where
  -- `showSDocUnsafe` will be fine so long as we've initialized the `unsafeGlobalDynFlags`
  -- which happens at the start of ghc-parser.
  toJSON x = toJSON $ showSDocUnsafe x

instance ToJSON RealSrcSpan where
  toJSON x = toJSON
    [ [srcSpanStartLine x, srcSpanStartCol x]
    , [srcSpanEndLine x, srcSpanEndCol x]
    ]

instance FromJSON RealSrcSpan where
  parseJSON v = parseJSON v >>= \case
    [[sL, sC], [eL, eC]] -> pure $ mkRealSrcSpan (mkRealSrcLoc "" sL sC) (mkRealSrcLoc "" eL eC)
    _ -> typeMismatch "RealSrcSpan should be a 2x2 array" v

instance ToJSON SrcSpan where
  toJSON srcSpan = case srcSpan of
    UnhelpfulSpan s -> object ["unhelpful" .= toJSON s]
    RealSrcSpan x -> toJSON x

instance FromJSON SrcSpan where
  parseJSON v =
        (UnhelpfulSpan <$> parseJSON v)
    <|> (RealSrcSpan <$> parseJSON v)

instance ToJSON FastString where
  toJSON str = toJSON $ unpackFS str

instance FromJSON FastString where
  parseJSON = withText "FastString" $ pure . mkFastString . T.unpack

data Failure = Failure SDoc SrcSpan

instance ToJSON Failure where
  toJSON (Failure sdoc srcSpan) = object ["failure" .= sdoc, "srcSpan" .= srcSpan]
