# Loan Datum JSON Schema:
```
data LoanDatum = LoanDatum
  { 
    usdAmount     :: Integer ,
    paramNFT      :: AssetClass ,
    loanToken     :: AssetClass ,
    loanStart     :: POSIXTime
  }
```
Please fill in the accurate values for usdAmount (USD amount * 10 ^ 6 for decimal). Loan token is simply the minting Currency Policy plus the token name (which is the treasury txhash (without index)). loanStart is the starting POSIX time of the loan (in milliseconds). This time has to be lower than the beginning of the validity interval.



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