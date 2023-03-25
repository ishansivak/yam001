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
import Ledger.Address
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show (..))
import qualified Prelude as Pr

{-
Potential additional params in the param datum:
maxLoan (definite)
additional stake keys
additional usd coins
treasuryValHash (definite)
finite interest repayment variable
-}
data ParamDatum = ParamDatum
<<<<<<< HEAD
  { 
    usdLL        :: Integer,             --oracle
    cblpLL       :: Integer,             --oracle
    arbValHash   :: ValidatorHash,       --arb
    upBool       :: Bool
=======
  { cRatio       :: Integer,   --6 digit CR e.g. 1.6 = 1600000
    usdLL        :: Integer,
    cblpLL       :: Integer,
    stake1       :: StakePubKeyHash,  --Will be staking script hash in final product
    usd1         :: AssetClass,
    usd1decimal  :: Integer,
    minLoan      :: Integer,
    maxLoan      :: Integer,
    loanValHash  :: PaymentPubKeyHash,
    arbValHash   :: PaymentPubKeyHash,
    trValHash    :: PaymentPubKeyHash
>>>>>>> main
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

{-instance Eq ParamDatum where
  {-# INLINEABLE (==) #-}
  ParamDatum {paramNFT = a} == ParamDatum {paramNFT = a'} = (a == a')-}



PlutusTx.unstableMakeIsData ''ParamDatum
PlutusTx.makeLift ''ParamDatum

data ParamTypes

instance ValidatorTypes ParamTypes where
  type DatumType ParamTypes = ParamDatum