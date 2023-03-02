{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Treasury.TreasuryCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Treasury.TreasuryValidator as Treasury
import Treasury.TreasuryTypes
import qualified Plutus.V1.Ledger.Scripts
import qualified Plutus.V1.Ledger.Value
import qualified Plutus.V2.Ledger.Api
import qualified Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import PlutusTx.Prelude
import Prelude (FilePath, IO)


writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

writeProjectTreasuryScript :: IO (Either (FileError ()) ())
writeProjectTreasuryScript =
  writeValidator "output/treasury.plutus" $
    Treasury.validator $
      TreasuryParam
        { cblpToken = Plutus.V1.Ledger.Value.assetClass "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784" "tCBLP"
        }

