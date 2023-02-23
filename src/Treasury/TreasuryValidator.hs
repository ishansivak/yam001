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

module Treasury.TreasuryValidator (validator) where


import Ledger (scriptHashAddress)
import qualified Ledger.Ada as Ada
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes, mkTypedValidator, mkTypedValidatorParam, mkUntypedValidator, validatorScript)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show (..))
import qualified Prelude as Pr
import Treasury.TreasuryTypes


{-# INLINEABLE treasuryValidator #-}
treasuryValidator :: TreasuryParam -> TreasuryDatum -> TreasuryRedeemer -> ScriptContext -> Bool
treasuryValidator tparam tdatum tredeemer tcontext = 
    case tredeemer of 
        Withdraw -> traceIfFalse "Withdrawal conditions not met!"   withdrawConditions
        Deposit  -> traceIfFalse "Deposit conditions not met!"      depositConditions
        Update   -> traceIfFalse "Update not authorized!"           updateConditions
    where
      info :: TxInfo
      info = scriptContextTxInfo tcontext

      ownInput :: TxOut
      ownInput = case findOwnInput tcontext of
        Nothing -> traceError "treasury input missing"
        Just i -> txInInfoResolved i

      ownOutput :: TxOut
      ownOutput = case getContinuingOutputs tcontext of
        [o] -> o -- There must be exactly ONE output UTXO
        _ -> traceError "expected exactly one treasury output"

      
      
      withdrawConditions :: Bool
      withdrawConditions = True 

      depositConditions :: Bool
      depositConditions = False   --Placeholder till depositing to UTxO's is implemented

      updateConditions :: Bool
      updateConditions = False















--The business logic ends here, what follows is boilerplate required for compiling the above validator into a plutus script.



typedValidator :: TreasuryParam -> TypedValidator TreasuryTypes
typedValidator tp = go tp
  where
    go =
      mkTypedValidatorParam @TreasuryTypes
        $$(PlutusTx.compile [||treasuryValidator||])
        $$(PlutusTx.compile [||wrap||])
    wrap = mkUntypedValidator

validator :: TreasuryParam -> Validator
validator = validatorScript . typedValidator