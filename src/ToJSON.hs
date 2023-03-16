{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module ToJSON where

import            Cardano.Api             (ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), scriptDataToJson)
import            Data.Aeson              (encode)
import            Data.String             (IsString(fromString))
import            Data.Either             (fromRight)
import            Data.Text               (Text, pack)
import            Ledger.Bytes            (LedgerBytes(LedgerBytes), fromHex)
import  qualified Ledger               as Lr
import            Ledger.Crypto           (PubKey(..))
import  qualified Data.ByteString.Lazy as LBS
import  qualified PlutusTx             
import            PlutusTx.Builtins.Internal
import            PlutusTx.Builtins.Class
import            Plutus.V1.Ledger.Value
import            Plutus.V1.Ledger.Api
import            Cardano.Api.Shelley     (fromPlutusData)

import            Prelude
import            Treasury.TreasuryTypes
import  qualified Loan.LoanTypes             as LnD
import            Param.ParamTypes
import            Treasury.TreasuryCompiler
import            Loan.LoanCompiler
import  qualified Treasury.TreasuryValidator as Tr
import  qualified Loan.LoanValidator         as Ln


loanDatum :: LnD.LoanDatum
loanDatum = LnD.LoanDatum {
    LnD.usdLoanToken  =  assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "tUSD",
    LnD.usdAmount     =  1000 ,
    LnD.paramNFT      =  assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "paramtoken"
}

testDatum :: TreasuryDatum
testDatum = TreasuryDatum {
      paramNFT = assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "paramtoken"
} 

--5820f0b8380a408c5cb2ab0db35db480baf8dadfcfa2a12bd0eaedd87a984c943eda

-- A sample public key
myPubKey :: PubKey
myPubKey = PubKey {getPubKey = LedgerBytes (encodeUtf8 "5820f0b8380a408c5cb2ab0db35db480baf8dadfcfa2a12bd0eaedd87a984c943eda")}

pkh1 :: PubKeyHash
pkh1 = Lr.pubKeyHash myPubKey




paramDatum :: ParamDatum
paramDatum = ParamDatum
  { cRatio       =   60,   --6 digit CR e.g. 1.6 = 1600000
    usdLL        =   1000000,
    cblpLL       =   10000,
    stake1       =   "4f7ff7e6fcf93cdf36aabbc0407d252f67003841389e37fe83ef381c",
    usd1         =   assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "tUSD",
    usd1decimal  =   1000000,
    minLoan      =   100,
    maxLoan      =   1000,
    loanValHash  =   Ln.lnValidatorHash lp ,
    arbValHash   =   pkh1,
    trValHash    =   Tr.trValidatorHash tp ,
    trStateToken =   assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "statetoken"
  }

updateRedeemer :: TreasuryRedeemer
updateRedeemer = Withdraw


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData


main :: IO ()
main = do
      writeJSON "output/newParam.json" paramDatum
      writeJSON "output/tTDatum.json" testDatum
      writeJSON "output/wR.json"      updateRedeemer
      writeJSON "output/lnDatum.json" loanDatum