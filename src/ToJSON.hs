{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module ToJSON where

import            Cardano.Api             (ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), scriptDataToJson)
import            Data.Aeson              (encode)
import  qualified Data.ByteString.Lazy as LBS
import  qualified PlutusTx
import            PlutusTx.Builtins.Internal
import            Plutus.V1.Ledger.Value
import            Plutus.V1.Ledger.Api
import            Cardano.Api.Shelley     (fromPlutusData)

import            Prelude
import            Treasury.TreasuryTypes
import            Param.ParamTypes


testDatum :: TreasuryDatum
testDatum = TreasuryDatum {
      paramNFT = assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "paramtoken"
} 


paramDatum :: ParamDatum
paramDatum = ParamDatum
  { cRatio       =   60,   --6 digit CR e.g. 1.6 = 1600000
    usdLL        =   1000000,
    cblpLL       =   10000,
    stake1       =   PubKeyHash {getPubKeyHash = "4f7ff7e6fcf93cdf36aabbc0407d252f67003841389e37fe83ef381c"},
    usd1         =   assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "tUSD",
    usd1decimal  =   1000000,
    minLoan      =   100,
    maxLoan      =   1000,
    loanValHash  =   PubKeyHash {getPubKeyHash = "d464987bfd2c3e2c3f48ad747d0a37f99be745b2f335f0802a07f689"},
    arbValHash   =   PubKeyHash {getPubKeyHash = "d464987bfd2c3e2c3f48ad747d0a37f99be745b2f335f0802a07f689"},
    trValHash    =   PubKeyHash {getPubKeyHash = "d464987bfd2c3e2c3f48ad747d0a37f99be745b2f335f0802a07f689"}
  }

updateRedeemer :: TreasuryRedeemer
updateRedeemer = Withdraw


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData


main :: IO ()
main = do
      writeJSON "output/tPDatum.json" paramDatum
      writeJSON "output/tTDatum.json" testDatum
      writeJSON "output/wR.json"      updateRedeemer