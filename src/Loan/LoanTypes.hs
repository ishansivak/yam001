{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Loan.LoanTypes
  ( LoanParam (..),
    LoanDatum (..),
    LoanRedeemer (..),
    LoanTypes,
    )
where

import GHC.Generics (Generic)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes, mkTypedValidator, mkTypedValidatorParam, mkUntypedValidator, validatorScript)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show (..))
import qualified Prelude as Pr


data LoanParam = LoanParam
  { 
    cblpToken    :: AssetClass,
    stake1       :: StakeValidatorHash,  --tr , ln
    usd1         :: AssetClass,          --tr , ln
    usd1decimal  :: Integer,              --tr , ln
    arbVlHash   :: ValidatorHash
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.makeLift ''LoanParam

-- consider representing Issuer with a token, instead of PKH
data LoanDatum = LoanDatum
  { 
    usdAmount     :: Integer ,
    paramNFT      :: AssetClass ,
    loanToken     :: AssetClass
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)
{-
instance Eq LoanDatum where
  {-# INLINEABLE (==) #-}
  LoanDatum {usdLoanToken = a} == LoanDatum {usdLoanToken = a'} = (a == a')
-}


PlutusTx.unstableMakeIsData ''LoanDatum
PlutusTx.makeLift ''LoanDatum



data LoanRedeemer = Withdraw | Update
  deriving (Show)
instance Eq LoanRedeemer where
  {-# INLINEABLE (==) #-}
  Withdraw == Withdraw  = True
  Update   == Update    = True
  _        == _         = False

PlutusTx.makeIsDataIndexed ''LoanRedeemer [('Withdraw, 0), ('Update, 1)]
PlutusTx.makeLift ''LoanRedeemer

data LoanTypes

instance ValidatorTypes LoanTypes where
  type DatumType LoanTypes = LoanDatum
  type RedeemerType LoanTypes = LoanRedeemer