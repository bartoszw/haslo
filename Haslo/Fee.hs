---------------------------------------------------------
--
-- Module        : Fee
-- Copyright     : Bartosz Wójcik (2010)
-- License       : All rights reserved
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- This module implements idea of loan fee. Provides with 'FeeType' data type
-- which represents different fee types.
-- What is a loan fee?
-- Fee is an ad hoc amount of money customer has to pay on top of usual instalments.
-- It may reflect various business needs, eg. can be a one shot payment for specific
-- services, can be a kind of insurance against early loan repayment, etc.
-- From calculation point of view there are two significiant fee kinds:
-- * fee which changes amount of one or more instalments
-- * fee which doesn't change instalments' amounts.
-- One may ask what brings fee that doesn't change instalments' amounts. Such fee only
-- changes distribution of interest of the loan. Usually it causes that interest is to be
-- paid earlier, what in turn gives creditor insurance that desipte of early repayment his
-- profit is secured.
---------------------------------------------------------
module Fee (FeeClass (..)
           ,FeeFinanced (..)
           ,FeeAsLateInterest (..)
           ,FeeLimitingFstInstalment (..)
           )

where

import BasicType 
import CalcConfigurationType (instList
                             ,initPrincipal
                             )
import Calculator (calcInstWithKth)
import CalcConstructors (InstalmentPlan
                        ,newLoanR
                        ,newLoanRIL)
import ErrorHandling
import Parameters (ParamMonad)

-- | Class providing standard behaviour of adding new costs on top of the loan
class FeeClass a where
    addFee :: a
           -> Amount -- ^ fee amount
           -> InstalmentPlan
           -> ValidMonad InstalmentPlan

    addFeeParam :: a
                -> Amount -- ^ fee amount
                -> InstalmentPlan
                -> ParamMonad InstalmentPlan

    addFeeParam ft fee ip = lift $ lift $ addFee ft fee ip

-- | Increases capital amount, doesn't change total sum instalment amounts.
data FeeFinanced = FeeFinanced
     deriving (Eq, Ord, Show)

instance FeeClass FeeFinanced where
    addFee _ fee ip = newLoanRIL 0 (c+fee) (instList ip)
           where c = initPrincipal ip

-- | Fee is put into 'InstalmentPlan' as late interest. Doesn't change total sum instalment amounts.
data FeeAsLateInterest = FeeAsLateInterest
     deriving (Eq, Ord, Show)

instance FeeClass FeeAsLateInterest where
    addFee _ fee ip = newLoanRIL (fromIntegral fee) c (instList ip)
           where c = initPrincipal ip

-- | @1st inst amount = min fee current_1st_inst_amount@
--   makes sense for FlatBalloon products
data FeeLimitingFstInstalment = FeeLimitingFstInstalment
     deriving (Eq, Ord, Show)

instance FeeClass FeeLimitingFstInstalment where
    addFee _ fee ip = newLoanRIL (fromIntegral newFst) c is
           where is = newFst : tail ips
                 ips = instList ip
                 c = initPrincipal ip
                 newFst = min fee (head ips)

