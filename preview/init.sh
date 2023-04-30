TEST="--testnet-magic 2"
addr1=$(cat w1/ps1.addr)
pol1="0c9d679170cad870e1cffe45bc685d2f8b903801dca58a9bbb621794"
TOKENNAME1=$(echo -n "paramtoken" | xxd -ps | tr -d '\n')
TOKENNAME2=$(echo -n "38cba573e7fe6260e9f706031fe776f1a5f57ed9c5b92714d5b35cc1087c4849" | xxd -ps | tr -d '\n')
TOKENUSD=$(echo -n "tUSD" | xxd -ps | tr -d '\n')
TOKENCBLP=$(echo -n "tCBLP" | xxd -ps | tr -d '\n')
statetoken=$(echo -n "statetoken" | xxd -ps | tr -d '\n')