-- | Definitions copied directly from 'Data.Typeable.Internal' which don't seem
--   to be re-exported anywhere else.

module Raehik.Data.Typeable.Internal.NotExported where

import Type.Reflection
import Data.Char ( ord, isDigit )

isListTyCon :: TyCon -> Bool
isListTyCon tc = tc == typeRepTyCon (typeRep :: TypeRep [])

isTupleTyCon :: TyCon -> Maybe (Bool, Int)
isTupleTyCon tc
  | tyConPackage tc == "ghc-prim"
  , tyConModule  tc == "GHC.Tuple" || tyConModule tc == "GHC.Types"
  = case tyConName tc of
      "Unit"  -> Just (True, 0)
      "Unit#" -> Just (False, 0)
      'T':'u':'p':'l':'e' : arity -> readTwoDigits arity
      _ -> Nothing
  | otherwise = Nothing

-- | See Note [Small Ints parsing] in GHC.Builtin.Types
readTwoDigits :: String -> Maybe (Bool, Int)
readTwoDigits s = case s of
  c1 : t1 | isDigit c1 -> case t1 of
    []    -> Just (True,  digit_to_int c1)
    ['#'] -> Just (False, digit_to_int c1)
    c2 : t2 | isDigit c2 ->
      let ar = digit_to_int c1 * 10 + digit_to_int c2
      in case t2 of
        []    -> Just (True, ar)
        ['#'] -> Just (False, ar)
        _ -> Nothing
    _ -> Nothing
  _ -> Nothing
  where
    digit_to_int :: Char -> Int
    digit_to_int c = ord c - ord '0'

-- This is only an approximation. We don't have the general
-- character-classification machinery here, so we just do our best.
-- This should work for promoted Haskell 98 data constructors and
-- for TypeOperators type constructors that begin with ASCII
-- characters, but it will miss Unicode operators.
--
-- If we wanted to catch Unicode as well, we ought to consider moving
-- GHC.Lexeme from ghc-boot-th to base. Then we could just say:
--
--   startsVarSym symb || startsConSym symb
--
-- But this is a fair deal of work just for one corner case, so I think I'll
-- leave it like this unless someone shouts.
isOperatorTyCon :: TyCon -> Bool
isOperatorTyCon tc
  | symb : _ <- tyConName tc
  , symb `elem` "!#$%&*+./<=>?@\\^|-~:" = True
  | otherwise                           = False
