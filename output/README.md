# Loan Datum JSON Schema:
```
data LoanDatum = LoanDatum
  { 
    usdAmount     :: Integer ,
    paramNFT      :: AssetClass ,
    loanToken     :: AssetClass
  }
```
Please fill in the accurate values for usdAmount (USD amount * 10 ^ 6 for decimal). Loan token is simply the minting Currency Policy plus the token name (which is the treasury txhash (without index)).



# Parameter Datum Schema:
```
data ParamDatum = ParamDatum
  { 
    usdLL        :: Integer,             --oracle
    cblpLL       :: Integer,             --oracle
    arbValHash   :: ValidatorHash,       --arb
    upBool       :: Bool
  }
```
I will update this datum, here usdLL stands for lovelace (10^-6 ADA) per USD token, which is a higher precision exchange rate between ADA and USD.
Same cblpLL. arbValHash refers to the validator hash of the arbitrage script (which is just tr script for now).



# Treasury Datum Schema:
```
data TreasuryDatum = TreasuryDatum
  { 
    paramNFT  :: AssetClass,
    nftSymbol :: CurrencySymbol
  }
```
nftSymbol refers to the Currency Symbol of the Loan NFTs.

Please let me know if you have any questions, comments,etc :).