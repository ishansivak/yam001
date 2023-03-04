{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Param.ParamTypes
  ( ParamDatum (..),
    ParamTypes
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

data ParamDatum = ParamDatum
  { cRatio       :: Integer,   --6 digit CR e.g. 1.6 = 1600000
    adaUSD       :: Integer,
    stake1       :: StakingCredential,
    usd1         :: AssetClass,
    usd1decimal  :: Integer,
    minLoan      :: Integer,
    loanValHash  :: Credential,
    arbValHash   :: Credential
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

{-instance Eq ParamDatum where
  {-# INLINEABLE (==) #-}
  ParamDatum {paramNFT = a} == ParamDatum {paramNFT = a'} = (a == a')-}



PlutusTx.unstableMakeIsData ''ParamDatum

data ParamTypes

instance ValidatorTypes ParamTypes where
  type DatumType ParamTypes = ParamDatum