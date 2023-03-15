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

tp :: TreasuryParam
tp = TreasuryParam{ cblpToken = Plutus.V1.Ledger.Value.assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "tCBLP"}

writeProjectTreasuryScript :: IO (Either (FileError ()) ())
writeProjectTreasuryScript =
  writeValidator "output/treasury001.plutus" $
    Treasury.validator $ tp
