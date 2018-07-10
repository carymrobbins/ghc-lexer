{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.GHC.Parser.Internal.JSON where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as T

import qualified ApiAnnotation
import BasicTypes
import FastString
import Lexer
import SrcLoc

locTokenToJSON (L srcSpan token) = object ["srcSpan" .= srcSpan, "token" .= token]

instance ToJSON C8.ByteString where
  toJSON bs = toJSON $ C8.unpack bs

instance ToJSON RealSrcSpan where
  toJSON x = object
    [ "startLine" .= srcSpanStartLine x
    , "startCol"  .= srcSpanStartCol  x
    , "endLine"   .= srcSpanEndLine   x
    , "endCol"    .= srcSpanEndCol    x
    ]

instance ToJSON SrcSpan where
  toJSON srcSpan = case srcSpan of
    UnhelpfulSpan s -> object ["unhelpful" .= toJSON s]
    RealSrcSpan x -> toJSON x

instance ToJSON FastString where
  toJSON str = toJSON $ unpackFS str

-- Later will want to use deriveJSON instead to get both ToJSON and FromJSON instances.
$(deriveToJSON defaultOptions ''FractionalLit)
$(deriveToJSON defaultOptions ''InlineSpec)
$(deriveToJSON defaultOptions ''SourceText)
$(deriveToJSON defaultOptions ''RuleMatchInfo)
$(deriveToJSON defaultOptions ''ApiAnnotation.IsUnicodeSyntax)
$(deriveToJSON defaultOptions ''ApiAnnotation.HasE)
$(deriveToJSON defaultOptions ''Token)
