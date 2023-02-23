{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Treasury.TreasuryTypes
  ( TreasuryParam (..),
    TreasuryDatum (..),
    TreasuryRedeemer (..),
    TreasuryTypes,
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


data TreasuryParam = TreasuryParam
  { tPolicyId :: CurrencySymbol,
    tName :: TokenName
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.makeLift ''TreasuryParam

-- consider representing Issuer with a token, instead of PKH
data TreasuryDatum = TreasuryDatum
  { govPolicyId :: CurrencySymbol,
    govTokenName :: TokenName
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.unstableMakeIsData ''TreasuryDatum


data TreasuryRedeemer = Withdraw | Deposit | Update
  deriving (Show)

PlutusTx.makeIsDataIndexed ''TreasuryRedeemer [('Withdraw, 0), ('Deposit, 1), ('Update, 2)]
PlutusTx.makeLift ''TreasuryRedeemer

data TreasuryTypes

instance ValidatorTypes TreasuryTypes where
  type DatumType TreasuryTypes = TreasuryDatum
  type RedeemerType TreasuryTypes = TreasuryRedeemer