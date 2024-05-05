{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- TODO for tmp shit

module Tmp where

import Data.Text.Builder.Linear qualified as TBL
import Data.Text.Builder.Linear.Buffer qualified as TBLB
import Type.Reflection
import Raehik.Data.Typeable.Internal.NotExported
import Data.Kind

import Data.Text ( Text )
import GHC.Types ( TyCon(..), TrName(..) )
import GHC.Exts ( IsString(fromString) )
import Raehik.Data.Text.Builder.Linear.ShowUtils

ptr :: forall a. Typeable a => Text
ptr = TBL.runBuilder $ prettyTypeRep 0 (typeRep @a)

prettyTypeRep :: Int -> TypeRep (a :: k) -> TBL.Builder
prettyTypeRep _ rep
  | Just HRefl <- rep `eqTypeRep` (typeRep :: TypeRep Type) =
    TBL.fromChar '*'

  | isListTyCon tc, [] <- tys =
    TBL.fromText "[]"
  | isListTyCon tc, [SomeTypeRep ty] <- tys =
    TBL.fromChar '[' <> prettyTypeRep 0 ty <> TBL.fromChar ']'

    -- Take care only to render saturated tuple tycon applications
    -- with tuple notation (#14341).
  | Just (boxed, n) <- isTupleTyCon tc,
    Just sat <- plainOrSaturated boxed n =
      tuple n boxed sat
  where
    --plainOrSaturated True _ | Just _ <- TrType `eqTypeRep` typeRepKind rep = Just True
    plainOrSaturated True _ | Just _ <- rep `eqTypeRep` (typeRep :: TypeRep Type) = Just True -- TODO idk just rewrote like text-show
    plainOrSaturated False n | n == length tys = Just True
    plainOrSaturated _ _ | [] <- tys = Just False
    plainOrSaturated _ _ | otherwise = Nothing

    (tc, tys) = splitApps rep

    tuple n boxed sat =
      let
        (lpar, rpar) = case boxed of
          True  -> (TBL.fromChar '(',  TBL.fromChar ')')
          False -> (TBL.fromText "(#", TBL.fromText "#)")
        commas = TBL.Builder (TBLB.prependChars (fromIntegral n - 1) ',')
        args = tshowArgs (TBL.fromChar ',') tys
        args' = case (boxed, sat) of
          (True,  True)  -> args
          (False, True)  -> TBL.fromChar ' ' <> args <> TBL.fromChar ' '
          (_,     False) -> commas
      in lpar <> args' <> rpar
prettyTypeRep _ (Con' tycon [])
  = prettyTyCon tycon
prettyTypeRep p (Con' tycon args)
  = tshowParen (p > 9) $
    prettyTyCon tycon <>
    TBL.fromChar ' ' <>
    tshowArgs (TBL.fromChar ' ') args
prettyTypeRep p (Fun x r)
  = tshowParen (p > 8) $
    prettyTypeRep 9 x <> " -> " <> prettyTypeRep 8 r
prettyTypeRep p (App f x)
  = tshowParen (p > 9) $
    prettyTypeRep 8 f <>
    TBL.fromChar ' ' <>
    prettyTypeRep 10 x

tshowArgs :: TBL.Builder -> [SomeTypeRep] -> TBL.Builder
tshowArgs _   []                       = mempty
tshowArgs _   [SomeTypeRep rep]        = prettyTypeRep 10 rep
tshowArgs sep (SomeTypeRep rep : reps) =
    prettyTypeRep 10 rep <> sep <> tshowArgs sep reps

showTyCon :: TyCon -> TBL.Builder
showTyCon tycon = tshowParen (isOperatorTyCon tycon) (prettyTyCon tycon)

prettyTyCon :: TyCon -> TBL.Builder
prettyTyCon (TyCon _ _ _ tc_name _ _) = prettyTrName tc_name

prettyTrName :: TrName -> TBL.Builder
prettyTrName = \case
  TrNameS s -> TBL.fromAddr   s
  TrNameD s -> fromString s

    {-
# if MIN_VERSION_base(4,20,0)
  | Just (boxed, n) <- isTupleTyCon tc,
    Just sat <- plainOrSaturated boxed n =
      tuple n boxed sat
# elif MIN_VERSION_base(4,19,0)
  | Just _ <- isTupleTyCon tc,
    Just _ <- typeRep @Type `eqTypeRep` typeRepKind rep =
    showbTuple tys
    -- Print (,,,) instead of Tuple4
  | Just n <- isTupleTyCon tc, [] <- tys =
      singleton '(' <> fromString (replicate (n-1) ',') <> singleton ')'
# else
  | isTupleTyCon tc
#  if MIN_VERSION_base(4,13,0)
  , Just _ <- typeRep @Type `eqTypeRep` typeRepKind rep
#  endif
  = showbTuple tys
# endif
-}

{-
prettyTypeRep p rep
    (TrTyCon {trTyCon = tycon, trKindVars = []})
  = showTyCon tycon
-}

{-
showTypeable _ (TrTyCon {trTyCon = tycon, trKindVars = []})
  = showTyCon tycon
showTypeable p (TrTyCon {trTyCon = tycon, trKindVars = args})
  = showParen (p > 9) $
    showTyCon tycon .
    showChar ' ' .
    showArgs (showChar ' ') args
showTypeable p (TrFun {trFunArg = x, trFunRes = r})
  = showParen (p > 8) $
    showsPrec 9 x . showString " -> " . showsPrec 8 r
showTypeable p (TrApp {trAppFun = f, trAppArg = x})
  = showParen (p > 9) $
    showsPrec 8 f .
    showChar ' ' .
    showsPrec 10 x
-}
