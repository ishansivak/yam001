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
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Api as PlutusV1
import qualified Plutus.V1.Ledger.Contexts as PlutusV1
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified Plutus.V2.Ledger.Contexts as PlutusV2
import qualified Plutus.Script.Utils.V2.Scripts as PV2
import PlutusTx (getPir)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (FilePath, IO, Show (..))
import qualified Prelude as Pr
import Param.ParamTypes
import qualified Loan.LoanTypes as Ln

data CBLPMintParams = CBLPMintParams
  { paramNFT :: !AssetClass
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.unstableMakeIsData ''CBLPMintParams
PlutusTx.makeLift ''CBLPMintParams


{-# INLINEABLE cblpMintPolicy #-}
cblpMintPolicy :: CBLPMintParams -> () -> PlutusV2.ScriptContext -> Bool
cblpMintPolicy amp mORb ctx = case PlutusV2.scriptContextPurpose ctx of
  PlutusV2.Minting cs      -> mintOrBurn
  _                        -> False

  
  
  
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    references :: [TxInInfo]
    references = PlutusV2.txInfoReferenceInputs info

    tOPs :: [TxOut]
    tOPs = txInfoOutputs info

    tIPs :: [TxOut]
    tIPs = txInInfoResolved (txInfoInputs info)


    ownCS :: PlutusV2.CurrencySymbol
    ownCS = PlutusV2.ownCurrencySymbol ctx

    prmAsset :: AssetClass
    prmAsset = paramNFT amp

    hasNFT :: [TxOut] -> TxOut
    hasNFT outs = case f of
                    [o] -> o
                    _   -> traceError "Exactly one param NFT input needed"
                  where
                    f = [x | x <- outs , 1 == (assetClassValueOf (txOutValue x) prmAsset)]
      
      
    paramOutput :: TxOut
    paramOutput = hasNFT [txInInfoResolved y | y <- references]  

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
    loanOutput = case [op | op <- tOPs , (txOutAddress op) == (scriptValidatorHashAddress (loanValHash pODatum) (Just (stake1 pODatum)))] of
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

    mintCondition :: Bool
    mintCondition = False
    
    burnCondition :: Bool
    burnCondition = False

{-
    As a Minting Policy
-}

policyV2 :: PSU.V2.MintingPolicy
policyV2 = PlutusV2.MintingPolicy $ PlutusV2.fromCompiledCode ($$(PlutusTx.compile [||wrap||]))
  where
    wrap amp = PSU.V2.mkUntypedMintingPolicy $ cblpMintPolicy (PlutusTx.unsafeFromBuiltinData amp)


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