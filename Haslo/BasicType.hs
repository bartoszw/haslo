---------------------------------------------------------
--
-- Module        : BasicType
-- Copyright     : Bartosz Wójcik (2010)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Basic types of Haskell Loan.
---------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-}
module Haslo.BasicType
where

import Text.PrettyShow


-- | Each amount has its own fix precision. Usually two digits after decimal point, sometimes
--   even till full hundrest or thousends. Therefore all amounts are stored as Ints. This prevents
--   situation where rounded amounts don't sum up to given value. Alternatilvely @Int@ would be 
--   suitable.
type Amount    = Integer

-- | Interest calculated. Interest paid is of @Amount@ data type.
type Interest  = Double

-- | Interest rate.
type Rate      = Double

-- | Duration of the loan.
type Duration  = Int


-- | Operations on @Amount@s have to be rounded to full amount. 
data RoundingType = Rounded
                  | Truncated
          deriving (Eq, Ord, Show, Enum)

instance PrettyShow RoundingType where
   showWithLen n = showWithLen n . show

