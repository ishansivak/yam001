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
                    traceIfFalse "minmaxLoanCondition not met!"                        minmaxLoanCondition &&
                    traceIfFalse "State token must be present in input and output!"    stateTokenCondition &&
                    traceIfFalse "The loan datum is not correct!"                      loanDatumCondition  &&
                    traceIfFalse "Loan NFT not minted!"                                mintNFTCondition

        --Withdrawal conditions above
        Deposit  -> traceIfFalse "Deposit conditions not met!"                         depositConditions
        Update   -> traceIfFalse "Update not authorized!"                              updateConditions
    where

      --Generally useful primitive data like TxInfo, continuing outputs,etc
      info :: TxInfo
      info = scriptContextTxInfo tcontext

      cblpAsset :: AssetClass
      cblpAsset = cblpToken tparam

      references :: [TxInInfo]
      references = txInfoReferenceInputs info

      ownInputTxIn :: TxInInfo
      ownInputTxIn = case findOwnInput tcontext of
        Nothing -> traceError "treasury input missing"
        Just i  ->  i
      
      ownInput :: TxOut
      ownInput = txInInfoResolved ownInputTxIn

      loanNFTName :: TokenName
      loanNFTName = TokenName (getTxId (txOutRefId (txInInfoOutRef ownInputTxIn)))
      
      loanNFTPolicy :: CurrencySymbol
      loanNFTPolicy = nftSymbol tdatum

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
      treasuryInputDatum = tdatum
      
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
<<<<<<< HEAD

      stateTokenCondition :: Bool
      stateTokenCondition = (1 == assetClassValueOf treasuryInputValue (trStateToken tparam)) &&
                            (1 == assetClassValueOf treasuryOutputValue (trStateToken tparam))  

      --Loan UTxO conditions defined below
      tOPs :: [TxOut]
      tOPs = txInfoOutputs info

      
{-

      lOutputsPay :: [TxOut]
      lOutputsPay = [op | op <- tOPs , (addressCredential (txOutAddress op)) == PubKeyCredential (loanValHash pODatum)]
=======

      --Loan UTxO conditions defined below
    {-
      txOuts :: [TxOut]
      txOuts = txInfoOutputs info

      addrParse :: TxOut -> (Credential, Maybe StakingCredential)
      addrParse tx = (a,b)
                     where
                      add = txOutAddress tx
                      a   = addressCredential add
                      b   = addressStakingCredential add

      lOutputs :: [TxOut]
      lOutputs = [op | op <- txOuts , (txOutAddress op) == (pubKeyHashAddress (loanValHash pODatum) (Just (stake1 pODatum)))]

>>>>>>> main

      loanOutput :: TxOut
      loanOutput = case lOutputsPay of
        [o]     ->  o
        []      -> traceError "No loan outputs!"
        _       ->  traceError "There must be exactly 1 loan output"
    -}
      loanOutput :: TxOut
      loanOutput =
        let ins =
              [ i
                | i <- txInfoOutputs info,
                  txOutAddress i == pubKeyHashAddress (loanValHash pODatum) (Just (stake1 pODatum))
              ]
         in case ins of
              [o] -> o
              _ -> traceError "expected exactly one loan output"   


      loanValue :: Value
      loanValue = txOutValue loanOutput

      loanValue :: Value
      loanValue = valueLockedBy info (loanValHash pODatum)
-}
      loanOutput :: TxOut
      loanOutput = case [op | op <- tOPs , (txOutAddress op) == (scriptValidatorHashAddress (loanValHash tparam) (Just (stake1 tparam)))] of
        [o]     -> o
        _       -> traceError "Expected exactly one loan output"
      
      
      lnDatum :: OutputDatum -> Maybe Ln.LoanDatum
      lnDatum md = do 
        case md of
          OutputDatum d -> fromBuiltinData (getDatum d)
          _             -> traceError "Datum not found"

      loanDatum :: Ln.LoanDatum
      loanDatum = case lnDatum $ txOutDatum loanOutput of
        Just ld    ->   ld
        _          ->   traceError "Loan datum is not the correct format!"


      loanValue :: Value
      loanValue = txOutValue loanOutput    

      adaAsset :: AssetClass
      adaAsset = AssetClass{unAssetClass = (adaSymbol , adaToken)}

      llLocked :: Integer
      llLocked = assetClassValueOf loanValue adaAsset   --Amount of lovelace locked by contract

      cblpLocked :: Integer
      cblpLocked = assetClassValueOf loanValue cblpAsset  --Amount of CBLP *10^6  (decimal point)

      usd1Asset :: AssetClass
      usd1Asset = usd1 tparam

      usd1Withdrawn :: Integer
      usd1Withdrawn = (assetClassValueOf treasuryInputValue usd1Asset) - (assetClassValueOf treasuryOutputValue usd1Asset)

      usd1Dec :: Integer
      usd1Dec = usd1decimal tparam
      
      valueMinted :: Value
      valueMinted = txInfoMint info
      
      --Beyond this point, all values are boolean conditions used in the final spending check
      
      
      valueToBeMinted :: Value
      valueToBeMinted = singleton loanNFTPolicy loanNFTName 1
      
      
      
      collateralCheck :: Bool
      collateralCheck = ((llLocked * usd1Dec) >= (usd1Withdrawn * (usdLL pODatum))) && ((cblpLocked * (cblpLL pODatum) * 100 * usd1Dec) >= (usd1Withdrawn * (usdLL pODatum) * 60))

      --Final formulation of withdraw spending conditions

      minmaxLoanCondition :: Bool
      minmaxLoanCondition = (usd1Withdrawn >= (minLoan tparam) * usd1Dec) && (usd1Withdrawn <= (maxLoan tparam) * usd1Dec)

      loanDatumCondition :: Bool
      loanDatumCondition = ((Ln.usdAmount loanDatum) >= usd1Withdrawn) &&
                           ((Ln.paramNFT loanDatum) == prmAsset) &&
                           ((Ln.loanToken loanDatum) == (assetClass loanNFTPolicy loanNFTName))

      datumCondition :: Bool
      datumCondition =  treasuryInputDatum == treasuryOutputDatum

<<<<<<< HEAD
      mintNFTCondition :: Bool
      mintNFTCondition = valueMinted == valueToBeMinted

      
      --Deposit conditions
      depositConditions :: Bool
      depositConditions = (1 == assetClassValueOf (valueSpent info) prmAsset)   --Placeholder till depositing to UTxO's is implemented
      --Update conditions
      

      updateConditions :: Bool
      updateConditions = (1 == assetClassValueOf (valueSpent info) prmAsset)
=======
      withdrawConditions :: Bool
      withdrawConditions = usd1Withdrawn >= minLoan pODatum
      
      
      
      
      
      --Deposit conditions
      depositConditions :: Bool
      depositConditions = False   --Placeholder till depositing to UTxO's is implemented
      
      
      --Update conditions


      updateConditions :: Bool
      updateConditions = True   --Placeholder till the withdraw logic is formalized
>>>>>>> main










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