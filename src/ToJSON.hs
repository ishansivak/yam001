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
import            Ledger.Address
import            Prelude
import            Treasury.TreasuryTypes
import  qualified Loan.LoanTypes             as LnD
import            Param.ParamTypes
<<<<<<< HEAD
import            Treasury.TreasuryCompiler
import            Loan.LoanCompiler
import  qualified Treasury.TreasuryValidator as Tr
import  qualified Loan.LoanValidator         as Ln
import  qualified Stake.StakeValidator       as Sv
import  qualified Arb.ArbValidator           as Ar
import  qualified Mint.MintValidator         as Mv
=======
import            Treasury.TreasuryValidator
>>>>>>> main


loanDatum :: LnD.LoanDatum
loanDatum = LnD.LoanDatum {
    LnD.usdAmount     =  100000000 ,
    LnD.paramNFT      =  assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "paramtoken" ,
    LnD.loanToken     =  assetClass Mv.policyCS "TXIDgoeshere"
}

testDatum :: TreasuryDatum
<<<<<<< HEAD
testDatum = TreasuryDatum 
  {
      paramNFT  = assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "paramtoken",
      nftSymbol = Mv.policyCS
  } 

--5820f0b8380a408c5cb2ab0db35db480baf8dadfcfa2a12bd0eaedd87a984c943eda

-- A sample public key
myPubKey :: PubKey
myPubKey = PubKey {getPubKey = LedgerBytes (encodeUtf8 "5820f0b8380a408c5cb2ab0db35db480baf8dadfcfa2a12bd0eaedd87a984c943eda")}

pkh1 :: PubKeyHash
pkh1 = Lr.pubKeyHash myPubKey


=======
testDatum = TreasuryDatum {
      paramNFT = assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "paramtoken"
} 
>>>>>>> main

paramDatum :: ParamDatum
paramDatum = ParamDatum
  { 
    usdLL        =   1000000,
    cblpLL       =   10000,
<<<<<<< HEAD
    arbValHash   =   Tr.trValidatorHash tp,
    upBool       =   False  
=======
    stake1       =   StakePubKeyHash { unStakePubKeyHash = (PubKeyHash {getPubKeyHash = encodeUtf8 $ stringToBuiltinString "4f7ff7e6fcf93cdf36aabbc0407d252f67003841389e37fe83ef381c"})},
    usd1         =   assetClass "16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016" "tUSD",
    usd1decimal  =   1000000,
    minLoan      =   100,
    maxLoan      =   1000,
    loanValHash  =   PaymentPubKeyHash{ unPaymentPubKeyHash = (PubKeyHash {getPubKeyHash = encodeUtf8 $ stringToBuiltinString "d464987bfd2c3e2c3f48ad747d0a37f99be745b2f335f0802a07f689"})},
    arbValHash   =   PaymentPubKeyHash{ unPaymentPubKeyHash = (PubKeyHash {getPubKeyHash = encodeUtf8 $ stringToBuiltinString "d464987bfd2c3e2c3f48ad747d0a37f99be745b2f335f0802a07f689"})},
    trValHash    =   PaymentPubKeyHash{ unPaymentPubKeyHash = (PubKeyHash {getPubKeyHash = encodeUtf8 $ stringToBuiltinString "d464987bfd2c3e2c3f48ad747d0a37f99be745b2f335f0802a07f689"})}
>>>>>>> main
  }

updateRedeemer :: TreasuryRedeemer
updateRedeemer = Withdraw


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData


main :: IO ()
main = do
<<<<<<< HEAD
      writeJSON "output/paramDatum.json"            paramDatum
      writeJSON "output/treasuryDatum.json"         testDatum
      writeJSON "output/treasuryRedeemer.json"      updateRedeemer
      writeJSON "output/loanRedeemer.json"          updateRedeemer
      writeJSON "output/arbDatum.json"              updateRedeemer
      writeJSON "output/mintRedeemer.json"          updateRedeemer
      writeJSON "output/loanDatum.json"             loanDatum
=======
      writeJSON "output/testParamDatum.json" paramDatum
      writeJSON "output/testTreasDatum.json" testDatum
      writeJSON "output/wRedeemer.json"      updateRedeemer
>>>>>>> main
