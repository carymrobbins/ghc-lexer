{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.GHC.Parser.Internal.JSON where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified ApiAnnotation
import BasicTypes
import FastString
import Lexer
import Outputable
import SrcLoc

$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''FractionalLit)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''InlineSpec)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''SourceText)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RuleMatchInfo)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ApiAnnotation.IsUnicodeSyntax)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ApiAnnotation.HasE)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Token)

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

instance ToJSON FastString where
  toJSON str = toJSON $ unpackFS str

instance FromJSON FastString where
  parseJSON = withText "FastString" $ pure . mkFastString . T.unpack

data Failure = Failure SDoc SrcSpan

instance ToJSON Failure where
  toJSON (Failure sdoc srcSpan) = object ["failure" .= sdoc, "srcSpan" .= srcSpan]
