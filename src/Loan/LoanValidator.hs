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


import Ledger (scriptHashAddress, scriptValidatorHashAddress)
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

        cblpAsset :: AssetClass
        cblpAsset = cblpToken lparam

        references :: [TxInInfo]
        references = txInfoReferenceInputs info

        ownInput :: TxOut
        ownInput = case findOwnInput lcontext of
          Nothing -> traceError "No inputs found"
          Just i -> txInInfoResolved i

        ownOutputZero :: Bool
        ownOutputZero = case getContinuingOutputs lcontext of
          [] -> True  --There should be no loan outputs
          _  -> False --In case there are any outputs, tx is invalid.
        
        lDatum :: OutputDatum -> Maybe LoanDatum
        lDatum md = do 
          case md of
            OutputDatum d -> fromBuiltinData (getDatum d)
            _             -> traceError "Datum not found"
        
        loanInputDatum :: LoanDatum
        loanInputDatum = ldatum
        
        prmAsset :: AssetClass
        prmAsset = paramNFT loanInputDatum


        --Looking for param UTxO using the param state token
        hasNFT :: [TxOut] -> TxOut
        hasNFT outs = case f of
                        [o] -> o
                        _   -> traceError "Exactly one param NFT input needed"
                      where
                        f = [x | x <- outs , 1 == (assetClassValueOf (txOutValue x) prmAsset)]
      
        paramOutput :: TxOut
        paramOutput = hasNFT [txInInfoResolved y | y <- references]

         --Fetching and using param datum
        pDatum :: OutputDatum -> Maybe ParamDatum
        pDatum md = do 
          case md of
            OutputDatum d -> fromBuiltinData (getDatum d)
            _             -> traceError "Datum not found"
      
        pODatum :: ParamDatum
        pODatum = case pDatum $ txOutDatum paramOutput of
          Just td -> td
          _       -> traceError "Output does not have param datum"





        --Arbitrage contract output search
        arbHash :: ValidatorHash
        arbHash = arbValHash lparam

        stakeH :: StakeValidatorHash
        stakeH = stake1 lparam

        tOPs :: [TxOut]
        tOPs = txInfoOutputs info

        arbOutput :: TxOut
        arbOutput = case [op | op <- tOPs , (txOutAddress op) == (scriptValidatorHashAddress (arbValHash lparam) (Just (stake1 lparam)))] of
          [o]    -> o
          _      -> traceError "exactly one arb output expected!"

        arbValue :: Value
        arbValue = txOutValue arbOutput

        usdAsset :: AssetClass
        usdAsset = usd1 lparam

        usdDec :: Integer
        usdDec = usd1decimal lparam

        arbCondition :: Bool
        arbCondition = (assetClassValueOf arbValue usdAsset) * 100 >= (usdAmount loanInputDatum) * 110
        
        burnCondition :: Bool
        burnCondition = True

        loanConditions :: Bool
        loanConditions = ownOutputZero && arbCondition

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