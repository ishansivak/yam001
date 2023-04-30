cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in $TXINj \
--tx-in $TXINj2 \
--tx-out $addr1+"20000000 + $amount3" \
--tx-out-inline-datum-file script/newParam.json \
--change-address $addr1 \
--protocol-params-file protocol.json \
--out-file mint.raw

cardano-cli transaction sign \
--signing-key-file $key2 \
$TEST \
--tx-body-file mint.raw \
--out-file lock.signed

cardano-cli transaction submit \
--tx-file lock.signed \
$TEST

cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in $TXIN#0 \
--tx-in $TXIN#1 \
--tx-out $addr1+"10000000 + 1 $pol1.$TOKENNAME1" \
--tx-out-inline-datum-file script/tPDatum.json \
--change-address $addr1 \
--protocol-params-file protocol.json \
--out-file tx.raw


cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in $TXIN \
--tx-in $TXIN2 \
--tx-in $TXIN3 \
--tx-in-collateral $TXIN \
--tx-out $addr1+"20000000 + $amount3" \
--tx-out-inline-datum-file script/newParam.json \
--change-address $addr1 \
--certificate-file test.del \
--certificate-script-file script/test-policy.plutus \
--certificate-redeemer-file script/wR.json \
--protocol-params-file protocol.json \
--out-file mint.raw


cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in $TXIN \
--tx-out $addr2+"3000000 + $mintUSD" \
--mint "$mintUSD" \
--mint-script-file policy/p1.script \
--change-address $addr1 \
--protocol-params-file protocol.json \
--out-file mint.raw

cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in $TXIN \
--tx-out $addr1+"24000000" \
--tx-out-reference-script-file complete/treasury3.plutus \
--change-address $addr1 \
--protocol-params-file protocol.json \
--out-file mint.raw