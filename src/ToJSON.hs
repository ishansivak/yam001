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
      paramNFT = assetClass "0c9d679170cad870e1cffe45bc685d2f8b903801dca58a9bbb621794" "yamparams"
} 

paramDatum :: ParamDatum
paramDatum = ParamDatum
  { cRatio       =   160,   --6 digit CR e.g. 1.6 = 1600000
    adaUSD       =   1,
    stake1       =   StakingHash (PubKeyCredential (PubKeyHash {getPubKeyHash = "379a9b045a4927bf8fd284166fef7fcd2b44e5811760c753ca049018"})),
    usd1         =   assetClass "0c9d679170cad870e1cffe45bc685d2f8b903801dca58a9bbb621794" "tUSD",
    usd1decimal  =   1000000,
    minLoan      =   100,
    loanValHash  =   PubKeyCredential (PubKeyHash {getPubKeyHash = "a98b930e7aa8c822666e0dca5442d000e128965cf7516e955af6486b"}),
    arbValHash   =   PubKeyCredential (PubKeyHash {getPubKeyHash = "a98b930e7aa8c822666e0dca5442d000e128965cf7516e955af6486b"})
  }



writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData


main :: IO ()
main = do
      writeJSON "output/testParamDatum.json" paramDatum