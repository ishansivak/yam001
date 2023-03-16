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

data CBLPStakeParam = CBLPStakeParam
  { stakeNFT :: !AssetClass
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.unstableMakeIsData ''CBLPStakeParam
PlutusTx.makeLift ''CBLPStakeParam


{-# INLINEABLE cblpStakeValidator #-}
cblpStakeValidator :: CBLPStakeParam -> Bool -> PlutusV2.ScriptContext -> Bool
cblpStakeValidator amp mORb ctx = case mORb of
    True            ->           traceIfFalse "Minting of loan token not approved"     mintCondition
    False           ->           traceIfFalse "Burning of loan token not approved"     burnCondition

  
  
  
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx


    mintCondition :: Bool
    mintCondition = True
    
    burnCondition :: Bool
    burnCondition = True

{-
    As a Minting Policy
-}

stakeV2 :: PSU.V2.StakeValidator
stakeV2 = PlutusV2.StakeValidator $ PlutusV2.fromCompiledCode ($$(PlutusTx.compile [||wrap||]))
  where
    wrap amp = PSU.V2.mkUntypedStakeValidator $ cblpStakeValidator (PlutusTx.unsafeFromBuiltinData amp)



{-
    As a Script
-}

scriptV2 :: PlutusV2.Script
scriptV2 = PlutusV2.unStakeValidatorScript stakeV2

{-
ValidatorHash
-}
stakeVHash :: PlutusV2.ValidatorHash
stakeVHash = PV2.validatorHash (PlutusV2.Validator scriptV2) 

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