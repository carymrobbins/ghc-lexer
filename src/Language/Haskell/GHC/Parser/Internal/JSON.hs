{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.GHC.Parser.Internal.JSON where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8

import qualified ApiAnnotation
import BasicTypes
import FastString
import Lexer
import Outputable
import SrcLoc

-- Later will want to use deriveJSON instead to get both ToJSON and FromJSON instances.
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''FractionalLit)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''InlineSpec)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''SourceText)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RuleMatchInfo)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ApiAnnotation.IsUnicodeSyntax)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ApiAnnotation.HasE)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Token)

locTokenToJSON (L srcSpan token) = object ["srcSpan" .= srcSpan, "token" .= token]

putJSON :: ToJSON a => a -> IO ()
putJSON = LC8.putStr . encode

putJSONLn :: ToJSON a => a -> IO ()
putJSONLn = LC8.putStrLn . encode

instance ToJSON C8.ByteString where
  toJSON bs = toJSON $ C8.unpack bs

instance ToJSON SDoc where
  -- `showSDocUnsafe` will be fine so long as we've initialized the `unsafeGlobalDynFlags`
  -- which happens at the start of ghc-parser.
  toJSON x = toJSON $ showSDocUnsafe x

instance ToJSON RealSrcSpan where
  toJSON x = toJSON
    [ [srcSpanStartLine x, srcSpanStartCol x]
    , [srcSpanEndLine x, srcSpanEndCol x]
    ]

instance ToJSON SrcSpan where
  toJSON srcSpan = case srcSpan of
    UnhelpfulSpan s -> object ["unhelpful" .= toJSON s]
    RealSrcSpan x -> toJSON x

instance ToJSON FastString where
  toJSON str = toJSON $ unpackFS str

data Failure = Failure SDoc SrcSpan

instance ToJSON Failure where
  toJSON (Failure sdoc srcSpan) = object ["failure" .= sdoc, "srcSpan" .= srcSpan]
