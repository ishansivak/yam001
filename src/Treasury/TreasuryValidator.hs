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

module Treasury.TreasuryValidator (validator , trValidatorHash) where


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
import Treasury.TreasuryTypes
import qualified Loan.LoanTypes as Ln
import Param.ParamTypes

{-# INLINEABLE treasuryValidator #-}
treasuryValidator :: TreasuryParam -> TreasuryDatum -> TreasuryRedeemer -> ScriptContext -> Bool
treasuryValidator tparam tdatum tredeemer tcontext = 
    case tredeemer of       
        Withdraw -> traceIfFalse "Input and output treasury datum must be the same!"   datumCondition &&
                    traceIfFalse "Withdrawal conditions unmet!"                        collateralCheck &&
                    traceIfFalse "minLoanCondition not met!"                           minLoanCondition &&
                    traceIfFalse "State token must be present in input and output!"    stateTokenCondition &&
                    traceIfFalse "The loan datum is not correct!"                      loanDatumCondition

        --Withdrawal conditions above
        Deposit  -> traceIfFalse "Deposit conditions not met!"                         depositConditions
        Update   -> traceIfFalse "Update not authorized!"                              updateConditions
    where

      --Generally useful primitive data like TxInfo, continuing outputs,etc
      info :: TxInfo
      info = scriptContextTxInfo tcontext

      paramAsset :: AssetClass
      paramAsset = cblpToken tparam

      references :: [TxInInfo]
      references = txInfoReferenceInputs info

      ownInput :: TxOut
      ownInput = case findOwnInput tcontext of
        Nothing -> traceError "treasury input missing"
        Just i -> txInInfoResolved i

      ownOutput :: TxOut
      ownOutput = case getContinuingOutputs tcontext of
        [o] -> o -- There must be exactly ONE output UTXO
        _ -> traceError "expected exactly one treasury output"
      

      
      --Helper function to convert the datum into a TreasuryDatum
      tDatum :: OutputDatum -> Maybe TreasuryDatum
      tDatum md = do 
        case md of
          OutputDatum d -> fromBuiltinData (getDatum d)
          _             -> traceError "Datum not found"
      
      --treasury  datum of ownInput and ownOutput
      treasuryInputDatum :: TreasuryDatum
      treasuryInputDatum = case tDatum $ txOutDatum ownInput of
        Just td -> td
        _       -> traceError "Input does not have treasury datum"
      
      prmAsset :: AssetClass
      prmAsset = paramNFT treasuryInputDatum
      
      treasuryOutputDatum :: TreasuryDatum
      treasuryOutputDatum = case tDatum $ txOutDatum ownOutput of
        Just td -> td
        _       -> traceError "Output does not have treasury datum"



      treasuryInputValue :: Value
      treasuryInputValue = txOutValue ownInput

      treasuryOutputValue :: Value
      treasuryOutputValue = txOutValue ownOutput

      

      --Making sure the protocol param UTxO is referenced
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

      stateTokenCondition :: Bool
      stateTokenCondition = (1 == assetClassValueOf treasuryInputValue (trStateToken pODatum)) &&
                            (1 == assetClassValueOf treasuryOutputValue (trStateToken pODatum))  

      --Loan UTxO conditions defined below
      tOPs :: [TxOut]
      tOPs = txInfoOutputs info

      
{-

      lOutputsPay :: [TxOut]
      lOutputsPay = [op | op <- tOPs , (addressCredential (txOutAddress op)) == PubKeyCredential (loanValHash pODatum)]

      loanOutput :: TxOut
      loanOutput = case lOutputsPay of
        [o]     ->  o
        _       ->  traceError "There must be exactly 1 loan output"
      
      loanValue :: Value
      loanValue = txOutValue loanOutput

      loanValue :: Value
      loanValue = valueLockedBy info (loanValHash pODatum)
-}
      loanOutput :: TxOut
      loanOutput = case [op | op <- tOPs , (txOutAddress op) == (scriptValidatorHashAddress (loanValHash pODatum) (Just (stake1 pODatum)))] of
        [o]     -> o
        _       -> traceError "Expected exactly one loan output"
      
      toLoanDatum :: OutputDatum -> Maybe Ln.LoanDatum
      toLoanDatum ld = do
        case ld of
          OutputDatum d -> fromBuiltinData (getDatum d)
          _             -> traceError "Datum not found"

      lnDatum :: Ln.LoanDatum
      lnDatum = case toLoanDatum $ txOutDatum loanOutput of
        Just lnD -> lnD
        _        -> traceError "Loan output does not have loan datum"

      loanValue :: Value
      loanValue = txOutValue loanOutput    

      adaAsset :: AssetClass
      adaAsset = AssetClass{unAssetClass = (adaSymbol , adaToken)}

      llLocked :: Integer
      llLocked = assetClassValueOf loanValue adaAsset   --Amount of lovelace locked by contract

      cblpLocked :: Integer
      cblpLocked = assetClassValueOf loanValue paramAsset  --Amount of CBLP *10^6  (decimal point)

      usd1Asset :: AssetClass
      usd1Asset = usd1 pODatum

      usd1Withdrawn :: Integer
      usd1Withdrawn = (assetClassValueOf treasuryInputValue usd1Asset) - (assetClassValueOf treasuryOutputValue usd1Asset)

      usd1Dec :: Integer
      usd1Dec = usd1decimal pODatum
      
      collateralCheck :: Bool
      collateralCheck = ((llLocked * usd1Dec) >= (usd1Withdrawn * (usdLL pODatum))) && ((cblpLocked * (cblpLL pODatum) * 100 * usd1Dec) >= (usd1Withdrawn * (usdLL pODatum)))

      --Final formulation of withdraw spending conditions

      minLoanCondition :: Bool
      minLoanCondition = usd1Withdrawn >= (minLoan pODatum) * usd1Dec

      loanDatumCondition :: Bool
      loanDatumCondition = ((Ln.usdLoanToken lnDatum) == usd1Asset) &&
                           ((Ln.usdAmount lnDatum) == usd1Withdrawn) &&
                           ((Ln.paramNFT lnDatum) == prmAsset)

      datumCondition :: Bool
      datumCondition =  treasuryInputDatum == treasuryOutputDatum

      mintNFTCondition :: Bool
      mintNFTCondition = True

      
      --Deposit conditions
      depositConditions :: Bool
      depositConditions = False   --Placeholder till depositing to UTxO's is implemented
      --Update conditions
      

      updateConditions :: Bool
      updateConditions = (1 == assetClassValueOf (valueSpent info) prmAsset)










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

trValidatorHash :: TreasuryParam -> ValidatorHash
trValidatorHash = validatorHash . typedValidator