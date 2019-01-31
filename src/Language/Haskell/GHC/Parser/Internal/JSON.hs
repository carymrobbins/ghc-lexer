{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.GHC.Parser.Internal.JSON where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Coerce
import Data.IORef (IORef)
import GHC.Generics
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.Haskell.TH.Syntax

import qualified ApiAnnotation
import qualified Bag
import qualified Class
import qualified CoAxiom
import qualified ConLike
import qualified CoreSyn
import qualified CostCentre
import qualified CostCentreState
import qualified DataCon
import qualified ForeignCall
import qualified FieldLabel
import qualified MkId
import qualified Module
import qualified Name
import qualified PatSyn
import qualified RdrName
import qualified TyCon
import qualified TyCoRep
import qualified Unique
import qualified UniqDFM
import qualified UniqSet
import qualified VarSet
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

import qualified GHCi.RemoteTypes

------------------------------------------------------
-- Lexer tokens --------------------------------------
------------------------------------------------------

$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''FractionalLit)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''InlineSpec)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''SourceText)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RuleMatchInfo)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ApiAnnotation.IsUnicodeSyntax)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ApiAnnotation.HasE)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''IntegralLit)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Token)

------------------------------------------------------
-- Parser nodes --------------------------------------
------------------------------------------------------

defaultToJSON :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
defaultToJSON = genericToJSON $ defaultOptions { sumEncoding = ObjectWithSingleField }

-- Aliases for the GHC parsing stage passes
-- Makes it much easier to derive JSON instances for the parser nodes.
type GP = GhcPass 'Parsed
type GR = GhcPass 'Renamed
type GT = GhcPass 'Typechecked

deriving instance Generic (HsModule pass)
instance ToJSON (HsModule GP) where toJSON = defaultToJSON

instance ToJSON HsDocString where
  toJSON v = object [ "HsDocString" .= unpackHDS v ]

deriving instance Generic WarningTxt
instance ToJSON WarningTxt where toJSON = defaultToJSON

deriving instance Generic (GenLocated l e)
instance (ToJSON l, ToJSON e) => ToJSON (GenLocated l e) where toJSON = defaultToJSON

instance ToJSON Module.ModuleName where
  toJSON v = object [ "ModuleName" .= Module.moduleNameString v ]

deriving instance Generic StringLiteral
instance ToJSON StringLiteral where toJSON = defaultToJSON

deriving instance Generic (HsDecl pass)
instance ToJSON (HsDecl GP) where toJSON = defaultToJSON
instance ToJSON (HsDecl GR) where toJSON = defaultToJSON

deriving instance Generic (ImportDecl pass)
instance ToJSON (ImportDecl GP) where toJSON = defaultToJSON

deriving instance Generic (IE pass)
instance ToJSON (IE GP) where toJSON = defaultToJSON
instance ToJSON (IE GR) where toJSON = defaultToJSON

deriving instance Generic (HsBindLR idL idR)
instance ToJSON (HsBindLR GP GP) where toJSON = defaultToJSON
instance ToJSON (HsBindLR GR GR) where toJSON = defaultToJSON

deriving instance Generic (RoleAnnotDecl pass)
instance ToJSON (RoleAnnotDecl GP) where toJSON = defaultToJSON
instance ToJSON (RoleAnnotDecl GR) where toJSON = defaultToJSON
instance ToJSON (RoleAnnotDecl GT) where toJSON = defaultToJSON

deriving instance Generic (SpliceDecl p)
instance ToJSON (SpliceDecl GP) where toJSON = defaultToJSON
instance ToJSON (SpliceDecl GR) where toJSON = defaultToJSON

deriving instance Generic NoExt
instance ToJSON NoExt where toJSON = defaultToJSON

deriving instance Generic (RuleDecls pass)
instance ToJSON (RuleDecls GP) where toJSON = defaultToJSON
instance ToJSON (RuleDecls GR) where toJSON = defaultToJSON

deriving instance Generic (FieldLabel.FieldLbl a)
instance (ToJSON a) => ToJSON (FieldLabel.FieldLbl a) where toJSON = defaultToJSON

deriving instance Generic (AnnDecl pass)
instance ToJSON (AnnDecl GP) where toJSON = defaultToJSON
instance ToJSON (AnnDecl GR) where toJSON = defaultToJSON

deriving instance Generic (IEWrappedName a)
instance (ToJSON a) => ToJSON (IEWrappedName a) where toJSON = defaultToJSON

deriving instance Generic (PatSynBind idL idR)
instance ToJSON (PatSynBind GP GP) where toJSON = defaultToJSON
instance ToJSON (PatSynBind GR GR) where toJSON = defaultToJSON

deriving instance Generic Role
instance ToJSON Role where toJSON = defaultToJSON

deriving instance Generic (HsSplice pass)
instance ToJSON (HsSplice GP) where toJSON = defaultToJSON
instance ToJSON (HsSplice GR) where toJSON = defaultToJSON

deriving instance Generic (RuleDecl pass)
instance ToJSON (RuleDecl GP) where toJSON = defaultToJSON
instance ToJSON (RuleDecl GR) where toJSON = defaultToJSON

deriving instance Generic (WarnDecls pass)
instance ToJSON (WarnDecls GP) where toJSON = defaultToJSON
instance ToJSON (WarnDecls GR) where toJSON = defaultToJSON

deriving instance Generic IEWildcard
instance ToJSON IEWildcard where toJSON = defaultToJSON

deriving instance Generic (GRHSs p body)
instance (ToJSON body) => ToJSON (GRHSs GP body) where toJSON = defaultToJSON
instance (ToJSON body) => ToJSON (GRHSs GR body) where toJSON = defaultToJSON

deriving instance Generic (HsExpr p)
instance ToJSON (HsExpr GP) where toJSON = defaultToJSON
instance ToJSON (HsExpr GR) where toJSON = defaultToJSON
instance ToJSON (HsExpr GT) where toJSON = defaultToJSON

deriving instance Generic RdrName.RdrName
instance ToJSON RdrName.RdrName where toJSON = defaultToJSON

deriving instance Generic (ForeignDecl pass)
instance ToJSON (ForeignDecl GP) where toJSON = defaultToJSON
instance ToJSON (ForeignDecl GR) where toJSON = defaultToJSON

deriving instance Generic (MatchGroup p body)
instance (ToJSON body) => ToJSON (MatchGroup GP body) where toJSON = defaultToJSON
instance (ToJSON body) => ToJSON (MatchGroup GR body) where toJSON = defaultToJSON

deriving instance Generic (DefaultDecl pass)
instance ToJSON (DefaultDecl GP) where toJSON = defaultToJSON
instance ToJSON (DefaultDecl GR) where toJSON = defaultToJSON

instance (ToJSON body) => ToJSON (Bag.Bag body) where
  toJSON = toJSON . Bag.bagToList

deriving instance Generic SpliceExplicitFlag
instance ToJSON SpliceExplicitFlag where toJSON = defaultToJSON

deriving instance Generic (Sig pass)
instance ToJSON (Sig GP) where toJSON = defaultToJSON
instance ToJSON (Sig GR) where toJSON = defaultToJSON

deriving instance Generic (DerivDecl pass)
instance ToJSON (DerivDecl GP) where toJSON = defaultToJSON
instance ToJSON (DerivDecl GR) where toJSON = defaultToJSON

deriving instance Generic (ABExport pass)
instance ToJSON (ABExport GP) where toJSON = defaultToJSON
instance ToJSON (ABExport GR) where toJSON = defaultToJSON

deriving instance Generic (AnnProvenance body)
instance (ToJSON body) => ToJSON (AnnProvenance body) where toJSON = defaultToJSON

deriving instance Generic (InstDecl pass)
instance ToJSON (InstDecl GP) where toJSON = defaultToJSON
instance ToJSON (InstDecl GR) where toJSON = defaultToJSON
instance ToJSON (InstDecl GT) where toJSON = defaultToJSON

deriving instance Generic (Pat pass)
instance ToJSON (Pat GP) where toJSON = defaultToJSON
instance ToJSON (Pat GR) where toJSON = defaultToJSON

deriving instance Generic (TyClDecl pass)
instance ToJSON (TyClDecl GP) where toJSON = defaultToJSON
instance ToJSON (TyClDecl GR) where toJSON = defaultToJSON
instance ToJSON (TyClDecl GT) where toJSON = defaultToJSON

deriving instance Generic (CoreSyn.Tickish id)
instance (ToJSON id) => ToJSON (CoreSyn.Tickish id) where toJSON = defaultToJSON

deriving instance Generic DocDecl
instance ToJSON DocDecl where toJSON = defaultToJSON

deriving instance Generic TcEvBinds
instance ToJSON TcEvBinds where toJSON = defaultToJSON

instance ToJSON Var where
  toJSON v = object [ "Var" .= object
    [ "name" .= varName v
    , "type" .= varType v
    , "unique" .= varUnique v
    ] ]

deriving instance Generic (HsConDetails arg rec)
instance (ToJSON arg, ToJSON rec) => ToJSON (HsConDetails arg rec) where
  toJSON = defaultToJSON

deriving instance Generic (HsSplicedThing pass)
instance ToJSON (HsSplicedThing GP) where toJSON = defaultToJSON
instance ToJSON (HsSplicedThing GR) where toJSON = defaultToJSON

deriving instance Generic (RuleBndr pass)
instance ToJSON (RuleBndr GP) where toJSON = defaultToJSON
instance ToJSON (RuleBndr GR) where toJSON = defaultToJSON

deriving instance Generic (WarnDecl pass)
instance ToJSON (WarnDecl GP) where toJSON = defaultToJSON
instance ToJSON (WarnDecl GR) where toJSON = defaultToJSON

deriving instance Generic (GRHS pass body)
instance (ToJSON body) => ToJSON (GRHS GP body) where toJSON = defaultToJSON
instance (ToJSON body) => ToJSON (GRHS GR body) where toJSON = defaultToJSON

deriving instance Generic (StmtLR idL idR body)
instance (ToJSON body) => ToJSON (StmtLR GP GP body) where toJSON = defaultToJSON
instance (ToJSON body) => ToJSON (StmtLR GR GR body) where toJSON = defaultToJSON

instance ToJSON Name.Name where
  toJSON v = object [ "Name" .= object
    [ "occ" .= Name.nameOccName v
    , "unique" .= Name.nameUnique v
    , "loc" .= Name.nameSrcSpan v
    ] ]

deriving instance Generic (HsImplicitBndrs pass body)
instance (ToJSON body) => ToJSON (HsImplicitBndrs GP body) where toJSON = defaultToJSON
instance (ToJSON body) => ToJSON (HsImplicitBndrs GR body) where toJSON = defaultToJSON

deriving instance Generic (Match pass body)
instance (ToJSON body) => ToJSON (Match GP body) where toJSON = defaultToJSON
instance (ToJSON body) => ToJSON (Match GR body) where toJSON = defaultToJSON

deriving instance Generic HsWrapper
instance ToJSON HsWrapper where toJSON = defaultToJSON

deriving instance Generic (RecordPatSynField a)
instance (ToJSON a) => ToJSON (RecordPatSynField a) where toJSON = defaultToJSON

deriving instance Generic SpliceDecoration
instance ToJSON SpliceDecoration where toJSON = defaultToJSON

deriving instance Generic (HsPatSynDir pass)
instance ToJSON (HsPatSynDir GP) where toJSON = defaultToJSON
instance ToJSON (HsPatSynDir GR) where toJSON = defaultToJSON

deriving instance Generic ThModFinalizers
instance ToJSON ThModFinalizers where toJSON = defaultToJSON

deriving instance Generic Activation
instance ToJSON Activation where toJSON = defaultToJSON

deriving instance Generic (HsLocalBindsLR idL idR)
instance ToJSON (HsLocalBindsLR GP GP) where toJSON = defaultToJSON
instance ToJSON (HsLocalBindsLR GR GR) where toJSON = defaultToJSON

deriving instance Generic (HsRecFields pass body)
instance (ToJSON body) => ToJSON (HsRecFields GP body) where toJSON = defaultToJSON
instance (ToJSON body) => ToJSON (HsRecFields GR body) where toJSON = defaultToJSON

deriving instance Generic (HsRecField' id arg)
instance (ToJSON id, ToJSON arg) => ToJSON (HsRecField' id arg) where
  toJSON = defaultToJSON

deriving instance Generic Module.Module
instance ToJSON Module.Module where toJSON = defaultToJSON

deriving instance Generic (HsWildCardBndrs pass thing)
instance (ToJSON thing) => ToJSON (HsWildCardBndrs GP thing) where toJSON = defaultToJSON
instance (ToJSON thing) => ToJSON (HsWildCardBndrs GR thing) where toJSON = defaultToJSON

instance ToJSON Name.OccName where
  toJSON v = object [ "OccName" .= object
    [ "space" .= Name.occNameSpace v
    , "string" .= Name.occNameString v
    ] ]

deriving instance Generic (AmbiguousFieldOcc pass)
instance ToJSON (AmbiguousFieldOcc GP) where toJSON = defaultToJSON
instance ToJSON (AmbiguousFieldOcc GR) where toJSON = defaultToJSON

deriving instance Generic (HsType pass)
instance ToJSON (HsType GP) where toJSON = defaultToJSON
instance ToJSON (HsType GR) where toJSON = defaultToJSON

deriving instance Generic Origin
instance ToJSON Origin where toJSON = defaultToJSON

deriving instance Generic (HsOverLit pass)
instance ToJSON (HsOverLit GP) where toJSON = defaultToJSON
instance ToJSON (HsOverLit GR) where toJSON = defaultToJSON

deriving instance Generic ForeignExport
instance ToJSON ForeignExport where toJSON = defaultToJSON

deriving instance Generic (HsLit pass)
instance ToJSON (HsLit GP) where toJSON = defaultToJSON
instance ToJSON (HsLit GR) where toJSON = defaultToJSON

deriving instance Generic ForeignImport
instance ToJSON ForeignImport where toJSON = defaultToJSON

deriving instance Generic (SyntaxExpr pass)
instance ToJSON (SyntaxExpr GP) where toJSON = defaultToJSON
instance ToJSON (SyntaxExpr GR) where toJSON = defaultToJSON

deriving instance Generic (BooleanFormula a)
instance (ToJSON a) => ToJSON (BooleanFormula a) where toJSON = defaultToJSON

deriving instance Generic (HsTupArg pass)
instance ToJSON (HsTupArg GP) where toJSON = defaultToJSON
instance ToJSON (HsTupArg GR) where toJSON = defaultToJSON

deriving instance Generic (FixitySig pass)
instance ToJSON (FixitySig GP) where toJSON = defaultToJSON
instance ToJSON (FixitySig GR) where toJSON = defaultToJSON

deriving instance Generic (DerivStrategy pass)
instance ToJSON (DerivStrategy GP) where toJSON = defaultToJSON
instance ToJSON (DerivStrategy GR) where toJSON = defaultToJSON

deriving instance Generic TcSpecPrags
instance ToJSON TcSpecPrags where toJSON = defaultToJSON

deriving instance Generic (ClsInstDecl pass)
instance ToJSON (ClsInstDecl GP) where toJSON = defaultToJSON
instance ToJSON (ClsInstDecl GR) where toJSON = defaultToJSON
instance ToJSON (ClsInstDecl GT) where toJSON = defaultToJSON

deriving instance Generic Type
instance ToJSON Type where toJSON = defaultToJSON

deriving instance Generic (FamEqn pass pats rhs)
instance (ToJSON pats, ToJSON rhs) => ToJSON (FamEqn GP pats rhs) where toJSON = defaultToJSON
instance (ToJSON pats, ToJSON rhs) => ToJSON (FamEqn GR pats rhs) where toJSON = defaultToJSON

deriving instance Generic (HsStmtContext id)
instance (ToJSON id) => ToJSON (HsStmtContext id) where toJSON = defaultToJSON

deriving instance Generic (HsMatchContext id)
instance (ToJSON id) => ToJSON (HsMatchContext id) where toJSON = defaultToJSON

deriving instance Generic (ArithSeqInfo pass)
instance ToJSON (ArithSeqInfo GP) where toJSON = defaultToJSON
instance ToJSON (ArithSeqInfo GR) where toJSON = defaultToJSON

deriving instance Generic InlinePragma
instance ToJSON InlinePragma where toJSON = defaultToJSON

deriving instance Generic OverlapMode
instance ToJSON OverlapMode where toJSON = defaultToJSON

deriving instance Generic (DataFamInstDecl pass)
instance ToJSON (DataFamInstDecl GP) where toJSON = defaultToJSON
instance ToJSON (DataFamInstDecl GR) where toJSON = defaultToJSON

deriving instance Generic (HsBracket pass)
instance ToJSON (HsBracket GP) where toJSON = defaultToJSON
instance ToJSON (HsBracket GR) where toJSON = defaultToJSON

deriving instance Generic (TyFamInstDecl pass)
instance ToJSON (TyFamInstDecl GP) where toJSON = defaultToJSON
instance ToJSON (TyFamInstDecl GR) where toJSON = defaultToJSON

deriving instance Generic (HsCmdTop pass)
instance ToJSON (HsCmdTop GP) where toJSON = defaultToJSON
instance ToJSON (HsCmdTop GR) where toJSON = defaultToJSON

deriving instance Generic ConLike.ConLike
instance ToJSON ConLike.ConLike where toJSON = defaultToJSON

deriving instance Generic (HsDataDefn pass)
instance ToJSON (HsDataDefn GP) where toJSON = defaultToJSON
instance ToJSON (HsDataDefn GR) where toJSON = defaultToJSON

deriving instance Generic CostCentre.CostCentre
instance ToJSON CostCentre.CostCentre where toJSON = defaultToJSON

deriving instance Generic UnboundVar
instance ToJSON UnboundVar where toJSON = defaultToJSON

deriving instance Generic Boxity
instance ToJSON Boxity where toJSON = defaultToJSON

deriving instance Generic (LHsQTyVars pass)
instance ToJSON (LHsQTyVars GP) where toJSON = defaultToJSON
instance ToJSON (LHsQTyVars GR) where toJSON = defaultToJSON

deriving instance Generic EvBind
instance ToJSON EvBind where toJSON = defaultToJSON

instance ToJSON Unique.Unique where
  toJSON v = object [ "Unique" .= object [ "key" .= Unique.getKey v ] ]

deriving instance Generic HsIPName
instance ToJSON HsIPName where toJSON = defaultToJSON

deriving instance Generic (FamilyDecl pass)
instance ToJSON (FamilyDecl GP) where toJSON = defaultToJSON
instance ToJSON (FamilyDecl GR) where toJSON = defaultToJSON

deriving instance Generic PendingRnSplice
instance ToJSON PendingRnSplice where toJSON = defaultToJSON

deriving instance Generic LexicalFixity
instance ToJSON LexicalFixity where toJSON = defaultToJSON

deriving instance Generic EvBindsVar
instance ToJSON EvBindsVar where toJSON = defaultToJSON

deriving instance Generic (ParStmtBlock idL idR)
instance ToJSON (ParStmtBlock GP GP) where toJSON = defaultToJSON
instance ToJSON (ParStmtBlock GR GR) where toJSON = defaultToJSON

deriving instance Generic TyCoRep.Coercion
instance ToJSON TyCoRep.Coercion where toJSON = defaultToJSON

deriving instance Generic PendingTcSplice
instance ToJSON PendingTcSplice where toJSON = defaultToJSON

deriving instance Generic (ApplicativeArg pass)
instance ToJSON (ApplicativeArg GP) where toJSON = defaultToJSON
instance ToJSON (ApplicativeArg GR) where toJSON = defaultToJSON

deriving instance Generic EvTerm
instance ToJSON EvTerm where toJSON = defaultToJSON

deriving instance Generic HsArrAppType
instance ToJSON HsArrAppType where toJSON = defaultToJSON

deriving instance Generic TransForm
instance ToJSON TransForm where toJSON = defaultToJSON

deriving instance Generic HsSyn.Fixity
instance ToJSON HsSyn.Fixity where toJSON = defaultToJSON

-- TODO: Really not sure how to deal with this one...
-- Will just stub in {"Q":[]} for now.
instance
  ToJSON
    (GHCi.RemoteTypes.ForeignRef (Language.Haskell.TH.Syntax.Q ())) where
  toJSON _ = object [ "Q" .= [] ]

deriving instance Generic (HsValBindsLR idL idR)
instance ToJSON (HsValBindsLR GP GP) where toJSON = defaultToJSON
instance ToJSON (HsValBindsLR GR GR) where toJSON = defaultToJSON

deriving instance Generic (HsIPBinds pass)
instance ToJSON (HsIPBinds GP) where toJSON = defaultToJSON
instance ToJSON (HsIPBinds GR) where toJSON = defaultToJSON

deriving instance Generic (FieldOcc pass)
instance ToJSON (FieldOcc GP) where toJSON = defaultToJSON
instance ToJSON (FieldOcc GR) where toJSON = defaultToJSON

deriving instance Generic Module.UnitId
instance ToJSON Module.UnitId where toJSON = defaultToJSON

deriving instance Generic (ConDeclField pass)
instance ToJSON (ConDeclField GP) where toJSON = defaultToJSON
instance ToJSON (ConDeclField GR) where toJSON = defaultToJSON

deriving instance Generic OverLitVal
instance ToJSON OverLitVal where toJSON = defaultToJSON

deriving instance Generic ForeignCall.CExportSpec
instance ToJSON ForeignCall.CExportSpec where toJSON = defaultToJSON

deriving instance Generic ForeignCall.CCallConv
instance ToJSON ForeignCall.CCallConv where toJSON = defaultToJSON

deriving instance Generic TcSpecPrag
instance ToJSON TcSpecPrag where toJSON = defaultToJSON

deriving instance Generic (TyVarBndr a b)
instance (ToJSON a, ToJSON b) => ToJSON (TyVarBndr a b) where toJSON = defaultToJSON

-- I wish there was a better way.
instance ToJSON Name.NameSpace where
  toJSON v = object [ "NameSpace" .= [ ns ] ]
    where
    ns | Name.isVarNameSpace v = "Var"
       | Name.isDataConNameSpace v = "Data"
       | Name.isTvNameSpace v = "Tv"
       | Name.isTcClsNameSpace v = "TcCls"
       | otherwise = "unknown" -- :(

deriving instance Generic (HsTyVarBndr pass)
instance ToJSON (HsTyVarBndr GP) where toJSON = defaultToJSON
instance ToJSON (HsTyVarBndr GR) where toJSON = defaultToJSON

deriving instance Generic ForeignCall.Safety
instance ToJSON ForeignCall.Safety where toJSON = defaultToJSON

deriving instance Generic SrcStrictness
instance ToJSON SrcStrictness where toJSON = defaultToJSON

deriving instance Generic (HsGroup pass)
instance ToJSON (HsGroup GP) where toJSON = defaultToJSON
instance ToJSON (HsGroup GR) where toJSON = defaultToJSON

deriving instance Generic (HsCmd pass)
instance ToJSON (HsCmd GP) where toJSON = defaultToJSON
instance ToJSON (HsCmd GR) where toJSON = defaultToJSON

deriving instance Generic (ConDecl pass)
instance ToJSON (ConDecl GP) where toJSON = defaultToJSON
instance ToJSON (ConDecl GR) where toJSON = defaultToJSON

deriving instance Generic (HsDerivingClause pass)
instance ToJSON (HsDerivingClause GP) where toJSON = defaultToJSON
instance ToJSON (HsDerivingClause GR) where toJSON = defaultToJSON

deriving instance Generic (FamilyInfo pass)
instance ToJSON (FamilyInfo GP) where toJSON = defaultToJSON
instance ToJSON (FamilyInfo GR) where toJSON = defaultToJSON

deriving instance Generic (FamilyResultSig pass)
instance ToJSON (FamilyResultSig GP) where toJSON = defaultToJSON
instance ToJSON (FamilyResultSig GR) where toJSON = defaultToJSON

deriving instance Generic (InjectivityAnn pass)
instance ToJSON (InjectivityAnn GP) where toJSON = defaultToJSON
instance ToJSON (InjectivityAnn GR) where toJSON = defaultToJSON
instance ToJSON (InjectivityAnn GT) where toJSON = defaultToJSON

deriving instance Generic (NHsValBindsLR pass)
instance ToJSON (NHsValBindsLR GP) where toJSON = defaultToJSON
instance ToJSON (NHsValBindsLR GR) where toJSON = defaultToJSON
instance ToJSON (NHsValBindsLR GT) where toJSON = defaultToJSON

deriving instance Generic (IPBind pass)
instance ToJSON (IPBind GP) where toJSON = defaultToJSON
instance ToJSON (IPBind GR) where toJSON = defaultToJSON
instance ToJSON (IPBind GT) where toJSON = defaultToJSON

deriving instance Generic (TyClGroup pass)
instance ToJSON (TyClGroup GP) where toJSON = defaultToJSON
instance ToJSON (TyClGroup GR) where toJSON = defaultToJSON
instance ToJSON (TyClGroup GT) where toJSON = defaultToJSON

-- deriving instance Generic X
-- instance ToJSON X where toJSON = defaultToJSON

-- deriving instance Generic (X pass)
-- instance ToJSON (X GP) where toJSON = defaultToJSON
-- instance ToJSON (X GR) where toJSON = defaultToJSON
-- instance ToJSON (X GT) where toJSON = defaultToJSON

-- TODO: Not really sure what I'm supposed to do with this, it's defined as
-- newtype MkId.DataConBoxer
--  = MkId.DCB ([Type]
--              -> [Var] -> UniqSupply.UniqSM ([Var], [CoreSyn.CoreBind]))
instance ToJSON MkId.DataConBoxer where
  toJSON = object [ "DataConBoxer" .= [] ]

-- TODO: I have no idea how to deal with IORef. Why is there an IORef in
-- the parse tree?
instance ToJSON (IORef VarSet.CoVarSet) where
  toJSON _ = object [ "IORef" .= ["CoVarSet"] ]
instance ToJSON (IORef EvBindMap) where
  toJSON _ = object [ "IORef" .= ["EvBindMap"] ]

-- TODO: Bypassing hidden constructor. Probably bad, but, meh. Maybe we can
-- do this instead of the custom ToJSON in a few cases.
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyCon.TyCon)

$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''NewHsTypeX)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ForeignCall.Header)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ArgFlag)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''PatSyn.PatSyn)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CostCentre.CCFlavour)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Name.OccEnv)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsTyLit)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''UniqSet.UniqSet)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsIBRn)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsSrcBang)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CImportSpec)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsTupleSort)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Promoted)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyCoRep.TyLit)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsWildCardInfo)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RdrName.GlobalRdrElt)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsRuleRn)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DataCon.DataCon)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''NewOrData)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DataDeclRn)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ForeignCall.CType)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsQTvsRn)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''UntypedSpliceFlavour)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DataCon.EqSpec)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DataCon.DataConRep)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DataCon.StrictnessMark)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsImplBang)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RdrName.Parent)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RdrName.ImportSpec)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RdrName.ImpDeclSpec)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CoAxiom.CoAxiom)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CoreSyn.Expr)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''FixityDirection)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Module.DefUnitId)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CostCentreState.CostCentreIndex)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''SrcUnpackedness)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ForeignCall.CCallTarget)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RdrName.ImpItemSpec)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''UniqDFM.UniqDFM)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CoAxiom.Branches)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CoAxiom.CoAxiomRule)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''EvTypeable)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Module.IndefUnitId)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Module.InstalledUnitId)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RecFlag)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyCon.RuntimeRepInfo)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Module.ComponentId)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyCoRep.UnivCoProvenance)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyCon.Injectivity)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''LeftOrRight)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Class)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CoercionHole)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyCon.FamTyConFlav)

-----------------------------------------
-- Other instances, types, functions
-----------------------------------------

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
  -- which happens at the start of ghc-lexer.
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
