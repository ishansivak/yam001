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
import qualified Plutus.V1.Ledger.Value as PLV
import qualified Plutus.V2.Ledger.Api
import qualified Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import PlutusTx.Prelude
import Prelude (FilePath, IO)
import qualified Loan.LoanValidator         as Ln
import qualified Loan.LoanCompiler          as LnC
import qualified Stake.StakeValidator       as Sv

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

tp :: TreasuryParam
tp = TreasuryParam
  { 
    cblpToken    =   PLV.assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "tCBLP",
    cRatio       =   60,
    stake1       =   Sv.stakeVHash,
    usd1         =   PLV.assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "tUSD",
    usd1decimal  =   1000000,
    minLoan      =   100,
    maxLoan      =   1000,
    loanValHash  =   Ln.lnValidatorHash LnC.lp ,
    trStateToken =   PLV.assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "statetoken"
  }

writeProjectTreasuryScript :: IO (Either (FileError ()) ())
writeProjectTreasuryScript =
<<<<<<< HEAD
  writeValidator "output/treasuryXP.plutus" $
    Treasury.validator $ tp
=======
  writeValidator "output/treasuryPreview.plutus" $
    Treasury.validator $
      TreasuryParam
        { cblpToken = Plutus.V1.Ledger.Value.assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "tCBLP"
        }

>>>>>>> main
