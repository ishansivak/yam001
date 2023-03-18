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


module Stake.StakeValidator
  ( serialisedScriptV2,
    scriptSBSV2,
    scriptV2,
    writeSerialisedScriptV2,
    stakeVHash,
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

data CBLPStakeParams = CBLPStakeParams
  { stakeNFT :: !AssetClass
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.unstableMakeIsData ''CBLPStakeParams
PlutusTx.makeLift ''CBLPStakeParams


{-# INLINEABLE cblpStakeValidator #-}
cblpStakeValidator :: CBLPStakeParams -> () -> PlutusV2.ScriptContext -> Bool
cblpStakeValidator cblpSP () ctx = case PlutusV2.scriptContextPurpose ctx of
    PlutusV2.Certifying _            -> traceIfFalse "Stake NFT not present in certifying tx!"     stakeCondition
    PlutusV2.Rewarding _             -> traceIfFalse "Stake NFT not present in rewarding tx!"      stakeCondition 
    _                                -> False

  
  
  
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    inputValue :: Value
    inputValue = PlutusV2.valueSpent info

    sNFT :: AssetClass
    sNFT = stakeNFT cblpSP

    stakeNFTAmount :: Integer
    stakeNFTAmount = assetClassValueOf inputValue sNFT

    stakeCondition :: Bool
    stakeCondition = stakeNFTAmount == 1
    

{-
    As a Staking Script



stakeV2 :: CBLPStakeParams -> PSU.V2.StakeValidator
stakeV2 csp = PlutusV2.StakeValidator $ PlutusV2.fromCompiledCode ($$(PlutusTx.compile [||wrap||]))
  where
    wrap csp = PSU.V2.mkUntypedStakeValidator $ cblpStakeValidator (PlutusTx.unsafeFromBuiltinData csp)
-}
policy :: CBLPStakeParams -> PSU.V2.StakeValidator
policy cblpPRM = PlutusV2.mkStakeValidatorScript $
    $$(PlutusTx.compile [|| \prm' -> PSU.V2.mkUntypedStakeValidator $ cblpStakeValidator prm' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode cblpPRM

tempStakeParam :: CBLPStakeParams
tempStakeParam = CBLPStakeParams {
    stakeNFT = assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "paramtoken"
}

stakeV2temp :: PSU.V2.StakeValidator
stakeV2temp = policy tempStakeParam
{-
    As a Script
-}



scriptV2 :: PlutusV2.Script
scriptV2 = PlutusV2.unStakeValidatorScript stakeV2temp

{-
ValidatorHash
-}
stakeVHash :: PlutusV2.StakeValidatorHash
stakeVHash = PV2.stakeValidatorHash stakeV2temp

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