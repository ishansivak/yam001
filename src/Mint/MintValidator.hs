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

data CBLPMintParams = CBLPMintParams
  { paramNFT :: !AssetClass
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.unstableMakeIsData ''CBLPMintParams
PlutusTx.makeLift ''CBLPMintParams


{-# INLINEABLE cblpMintPolicy #-}
cblpMintPolicy :: CBLPMintParams -> Bool -> PlutusV2.ScriptContext -> Bool
cblpMintPolicy amp mORb ctx = case mORb of
    True            ->           traceIfFalse "Minting of loan token not approved"     mintCondition
    False           ->           traceIfFalse "Burning of loan token not approved"     burnCondition

  
  
  
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx


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
writeSerialisedScriptV2 = void $ writeFileTextEnvelope "output/test-policy.plutus" Nothing serialisedScriptV2