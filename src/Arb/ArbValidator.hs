{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Arb.ArbValidator (arbVHash, validator) where

import Plutus.V2.Ledger.Api (BuiltinData, Validator, mkValidatorScript, ValidatorHash)
import PlutusTx (compile, applyCode, liftCode, unstableMakeIsData, makeLift)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Contexts
import Plutus.Script.Utils.V2.Typed.Scripts
import qualified Ledger as L
import PlutusTx.Prelude hiding (Semigroup (..), unless)



data ArbParams = ArbParams
  {
    prmNFT :: AssetClass
  }
unstableMakeIsData ''ArbParams
makeLift ''ArbParams
{-# INLINEABLE arbValidator #-}
arbValidator :: ArbParams -> () -> () -> ScriptContext -> Bool
arbValidator arbprm () () ctx = traceIfFalse "nft not in input" nftCondition
  where 
    info :: TxInfo
    info = scriptContextTxInfo ctx

    vSpent :: Value
    vSpent = valueSpent info

    pNFT :: AssetClass
    pNFT = prmNFT arbprm

    nftCondition :: Bool
    nftCondition = ((assetClassValueOf vSpent pNFT) == 1)


validatorF :: ArbParams -> Validator
validatorF prm = mkValidatorScript $
    $$(compile [|| \prm' -> mkUntypedValidator $ arbValidator prm' ||])
    `applyCode`
    liftCode prm

tmpArb :: ArbParams
tmpArb = ArbParams {prmNFT = assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "paramtoken"}

validator :: Validator
validator = validatorF tmpArb

arbVHash :: ValidatorHash
arbVHash = L.validatorHash validator