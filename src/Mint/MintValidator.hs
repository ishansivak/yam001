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


module Mint.MintValidator
  ( serialisedScriptV2,
    scriptSBSV2,
    scriptV2,
    writeSerialisedScriptV2,
    policyCS
  )
where

import Cardano.Api
  ( PlutusScriptV1,
    PlutusScriptV2,
    writeFileTextEnvelope,
  )
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Functor (void)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Ledger (scriptHashAddress, scriptValidatorHashAddress)
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Api as PlutusV1
import qualified Plutus.V1.Ledger.Contexts as PlutusV1
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified Plutus.V2.Ledger.Contexts as PlutusV2
import qualified Plutus.V1.Ledger.Value as PLV
import qualified Plutus.Script.Utils.V2.Scripts as PV2
import PlutusTx (getPir)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified PlutusTx.AssocMap as Map
import Prelude (FilePath, IO, Show (..))
import qualified Prelude as Pr
import qualified Loan.LoanTypes as Ln
import qualified Loan.LoanCompiler as LnC
import qualified Loan.LoanValidator as LnV
import qualified Treasury.TreasuryTypes as Tr
import qualified Treasury.TreasuryCompiler as TrC
import qualified Treasury.TreasuryValidator as TrV
import qualified Stake.StakeValidator as Sv

data CBLPMintParams = CBLPMintParams
  { 
    trValHash     :: PV2.ValidatorHash,
    loanValHash   :: PV2.ValidatorHash,
    stakeVHash    :: PV2.StakeValidatorHash
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.unstableMakeIsData ''CBLPMintParams
PlutusTx.makeLift ''CBLPMintParams


{-# INLINEABLE cblpMintPolicy #-}
cblpMintPolicy :: CBLPMintParams -> () -> PlutusV2.ScriptContext -> Bool
cblpMintPolicy mparam () ctx = case PlutusV2.scriptContextPurpose ctx of
  PlutusV2.Minting _      -> mintCondition
  _                        -> False

  
  
  
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx
{-
    references :: [TxInInfo]
    references = PlutusV2.txInfoReferenceInputs info

    tOPs :: [TxOut]
    tOPs = txInfoOutputs info

    tIPs :: [TxOut]
    tIPs = PlutusV2.txInInfoResolved (txInfoInputs info)


    ownCS :: PlutusV2.CurrencySymbol
    ownCS = PlutusV2.ownCurrencySymbol ctx

    prmAsset :: AssetClass
    prmAsset = paramNFT mparam

    hasNFT :: [TxOut] -> TxOut
    hasNFT outs = case f of
                    [o] -> o
                    _   -> traceError "Exactly one param NFT input needed"
                  where
                    f = [x | x <- outs , 1 == (assetClassValueOf (txOutValue x) prmAsset)]
      
      
    paramOutput :: TxOut
    paramOutput = hasNFT [PlutusV2.txInInfoResolved y | y <- references]  

    pDatum :: OutputDatum -> Maybe ParamDatum
    pDatum md = do 
      case md of
        OutputDatum d -> fromBuiltinData (getDatum d)
        _             -> traceError "Datum not found"
      
    pODatum :: ParamDatum
    pODatum = case pDatum $ txOutDatum paramOutput of
      Just td -> td
      _       -> traceError "Output does not have param datum"

    loanOutput :: TxOut
    loanOutput = case [op | op <- tOPs , (PlutusV2.txOutAddress op) == (scriptValidatorHashAddress (loanValHash pODatum) (Just (stake1 pODatum)))] of
      [o]     -> o
      _       -> traceError "Expected exactly one loan output"

    toLoanDatum :: OutputDatum -> Maybe Ln.LoanDatum
    toLoanDatum ld = do
      case ld of
        OutputDatum d -> fromBuiltinData (getDatum d)
        _             -> traceError "Datum not found"
    
    trInpValue :: Value

    lnDatum :: Ln.LoanDatum
    lnDatum = case toLoanDatum $ txOutDatum loanOutput of
      Just lnD -> lnD
      _        -> traceError "Loan output does not have loan datum"
-}
    inInfo :: [PlutusV2.TxInInfo]
    inInfo = PlutusV2.txInfoInputs info

    trHash :: PV2.ValidatorHash
    trHash = trValHash mparam

    lnHash :: PV2.ValidatorHash
    lnHash = loanValHash mparam

    stkHash :: PV2.StakeValidatorHash
    stkHash = stakeVHash mparam

    trInputRef :: PlutusV2.TxOutRef
    trInputRef = case [PlutusV2.txInInfoOutRef x | x <- inInfo , (scriptValidatorHashAddress trHash (Just stkHash)) == (PlutusV2.txOutAddress (PlutusV2.txInInfoResolved x))] of
      [o]        -> o
      _          -> traceError "no treasury input"
    
    lnInputRef :: PlutusV2.TxOutRef
    lnInputRef = case [PlutusV2.txInInfoOutRef x | x <- inInfo , (scriptValidatorHashAddress lnHash (Just stkHash)) == (PlutusV2.txOutAddress (PlutusV2.txInInfoResolved x))] of
      [o]        -> o
      _          -> traceError "no loan output"

    valueMinted :: PlutusV2.Value
    valueMinted = PlutusV2.txInfoMint info

    redeemers :: PlutusV2.Map PlutusV2.ScriptPurpose PlutusV2.Redeemer
    redeemers = PlutusV2.txInfoRedeemers info

    valueSplitMinted :: (PlutusV2.Value, PlutusV2.Value)
    valueSplitMinted = PLV.split valueMinted
    
    boolSplitMinted :: (PlutusV2.Value, PlutusV2.Value) -> (Bool, Bool)
    boolSplitMinted (a,b) = (isZero a, isZero b)

    trMintCondition :: Bool
    trMintCondition = case (Map.lookup (PlutusV2.Spending trInputRef) redeemers) of 
      Just a              -> (PlutusV2.fromBuiltinData (PlutusV2.getRedeemer a)) == Just Tr.Withdraw
      _                   -> False
    
    lnBurnCondition :: Bool
    lnBurnCondition = case (Map.lookup (PlutusV2.Spending lnInputRef) redeemers) of 
      Just a              -> (PlutusV2.fromBuiltinData (PlutusV2.getRedeemer a)) == Just Ln.Withdraw
      _                   -> False

    mintCondition :: Bool
    mintCondition = case boolSplitMinted valueSplitMinted of
      (False , True) -> lnBurnCondition
      (True , False) -> trMintCondition
      _                                -> traceIfFalse "Neither minting nor burning condition satisfied" False
    


{-
    As a Minting Policy
-}

policy :: CBLPMintParams -> PSU.V2.MintingPolicy
policy cblpPRM = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \prm' -> PSU.V2.mkUntypedMintingPolicy $ cblpMintPolicy prm' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode cblpPRM

tmpMintParam :: CBLPMintParams
tmpMintParam = CBLPMintParams
  {
    trValHash    = TrV.trValidatorHash TrC.tp ,
    loanValHash  = LnV.lnValidatorHash LnC.lp ,
    stakeVHash   = Sv.stakeVHash
  }

policyV2 :: PSU.V2.MintingPolicy
policyV2 = policy tmpMintParam


{-
CurrencySymbol
-}
policyCS :: PlutusV2.CurrencySymbol
policyCS = PV2.scriptCurrencySymbol policyV2 

{-
    As a Script
-}

scriptV2 :: PlutusV2.Script
scriptV2 = PlutusV2.unMintingPolicyScript policyV2

{-
    As a Short Byte String
-}

scriptSBSV2 :: SBS.ShortByteString
scriptSBSV2 = SBS.toShort . LBS.toStrict $ serialise scriptV2

{-
    As a Serialised Script
-}

serialisedScriptV2 :: PlutusScript PlutusScriptV2
serialisedScriptV2 = PlutusScriptSerialised scriptSBSV2

writeSerialisedScriptV2 :: IO ()
writeSerialisedScriptV2 = void $ writeFileTextEnvelope "output/mint1.plutus" Nothing serialisedScriptV2