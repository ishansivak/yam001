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

module Loan.LoanValidator (lnValidator , lnValidatorHash) where


import Ledger (scriptHashAddress)
import qualified Ledger.Ada as Ada
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes, mkTypedValidator, mkTypedValidatorParam, mkUntypedValidator, validatorScript, validatorHash)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show (..))
import qualified Prelude as Pr
import qualified Treasury.TreasuryTypes as TrTypes
import Loan.LoanTypes
import Param.ParamTypes

{-# INLINEABLE loanValidator #-}
loanValidator :: LoanParam -> LoanDatum -> LoanRedeemer -> ScriptContext -> Bool
loanValidator lparam ldatum lredeemer lcontext =
    case lredeemer of 
        Withdraw       ->    traceIfFalse "Loan repayment conditions not met!"     loanConditions
        Update         ->    traceIfFalse "Update conditions unmet!"               loanUpdateConditions
    where
        info :: TxInfo
        info = scriptContextTxInfo lcontext

        

        loanConditions :: Bool
        loanConditions = True

        loanUpdateConditions :: Bool
        loanUpdateConditions = True


typedValidator :: LoanParam -> TypedValidator LoanTypes
typedValidator tp = go tp
  where
    go =
      mkTypedValidatorParam @LoanTypes
        $$(PlutusTx.compile [||loanValidator||])
        $$(PlutusTx.compile [||wrap||])
    wrap = mkUntypedValidator

lnValidator :: LoanParam -> Validator
lnValidator = validatorScript . typedValidator

lnValidatorHash :: LoanParam -> ValidatorHash
lnValidatorHash = validatorHash . typedValidator