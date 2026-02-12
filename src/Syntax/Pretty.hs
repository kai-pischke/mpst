-- | Pretty-printers for global and local session types.
module Syntax.Pretty
  ( prettyGlobalType
  , prettyLocalType
  , renderGlobalType
  , renderLocalType
  ) where

import qualified Data.List.NonEmpty as NE
import Prettyprinter
  ( Doc
  , colon
  , comma
  , defaultLayoutOptions
  , encloseSep
  , hsep
  , layoutPretty
  , pretty
  , space
  , (<+>)
  )
import Prettyprinter.Render.String (renderString)
import Syntax.AST

-- | Pretty-print a global type as a document.
prettyGlobalType :: GlobalType -> Doc ann
prettyGlobalType (GMessage p q bs) =
  hsep [prettyParticipant p, pretty "->", prettyParticipant q, prettyBranches prettyGlobalType bs]
prettyGlobalType (GVar v) = prettyTypeVar v
prettyGlobalType (GRec v g) = hsep [pretty "rec", prettyTypeVar v, pretty ".", prettyGlobalType g]
prettyGlobalType GEnd = pretty "end"

-- | Pretty-print a local type as a document.
prettyLocalType :: LocalType -> Doc ann
prettyLocalType (LSend p bs) = hsep [prettyParticipant p, pretty "!", prettyBranches prettyLocalType bs]
prettyLocalType (LRecv p bs) = hsep [prettyParticipant p, pretty "?", prettyBranches prettyLocalType bs]
prettyLocalType (LVar v) = prettyTypeVar v
prettyLocalType (LRec v t) = hsep [pretty "rec", prettyTypeVar v, pretty ".", prettyLocalType t]
prettyLocalType LEnd = pretty "end"

-- | Render a global type to a 'String'.
renderGlobalType :: GlobalType -> String
renderGlobalType = renderDoc . prettyGlobalType

-- | Render a local type to a 'String'.
renderLocalType :: LocalType -> String
renderLocalType = renderDoc . prettyLocalType

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty defaultLayoutOptions

prettyParticipant :: Participant -> Doc ann
prettyParticipant (Participant p) = pretty p

prettyLabel :: Label -> Doc ann
prettyLabel (Label l) = pretty l

prettyTypeVar :: TypeVar -> Doc ann
prettyTypeVar (TypeVar t) = pretty t

prettyBranches :: (a -> Doc ann) -> Branches a -> Doc ann
prettyBranches renderElem branches =
  encloseSep (pretty "{") (pretty "}") (comma <> space) (branchDoc <$> NE.toList branches)
  where
    branchDoc (lbl, t) = prettyLabel lbl <> colon <+> renderElem t
