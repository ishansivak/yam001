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
import            Cardano.Api.Shelley     (fromPlutusData)

import            Prelude
import            Treasury.TreasuryTypes


testDatum :: TreasuryDatum
testDatum = TreasuryDatum {
      paramNFT = assetClass "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784" "yamparams"
}        



writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData


main :: IO ()
main = do
      writeJSON "output/testF.json" testDatum