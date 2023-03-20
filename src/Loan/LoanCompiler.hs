{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Loan.LoanCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Loan.LoanValidator as Loan
import Loan.LoanTypes
import qualified Plutus.V1.Ledger.Scripts
import qualified Plutus.V1.Ledger.Value as PLV
import qualified Plutus.V2.Ledger.Api
import qualified Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import PlutusTx.Prelude
import Prelude (FilePath, IO)
import qualified Stake.StakeValidator       as Sv
import qualified Arb.ArbValidator           as Ar


writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

lp :: LoanParam
lp = LoanParam 
  { 
    cblpToken    =   PLV.assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "tCBLP",
    stake1       =   Sv.stakeVHash,
    usd1         =   PLV.assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "tUSD",
    usd1decimal  =   1000000,
    arbValHash   =   Ar.arbVHash
  }

writeProjectLoanScript :: IO (Either (FileError ()) ())
writeProjectLoanScript =
  writeValidator "output/loanXP.plutus" $
    Loan.lnValidator $ lp