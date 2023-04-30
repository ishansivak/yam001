cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in $TXIN \
--tx-out $addr1+"10000000 + 1 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.706172616d746f6b656e" \
--tx-out-inline-datum-file datum/testParamDatum.json \
--change-address $addr1 \
--protocol-params-file protocol.json \
--out-file param.raw