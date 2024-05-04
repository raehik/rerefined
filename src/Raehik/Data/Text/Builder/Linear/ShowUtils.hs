-- | 'Show'-like utils for text-builder-linear.

module Raehik.Data.Text.Builder.Linear.ShowUtils where

import Data.Text.Builder.Linear qualified as TBL

tshowParen :: Bool -> TBL.Builder -> TBL.Builder
tshowParen b p = if b then TBL.fromChar '(' <> p <> TBL.fromChar ')' else p
