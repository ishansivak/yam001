cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in $TXINx \
--tx-in $TXINz \
--read-only-tx-in-reference $TXREF \
--tx-in $TXIN2 \
--tx-in-script-file script/treasury001.plutus \
--tx-in-inline-datum-present \
--tx-in-redeemer-file script/uR.json \
--tx-in-collateral $TXINx \
--tx-out $addr2+"3000000 + $amountCBLP" \
--tx-out $addr2+"3000000 + 100000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.74555344" \
--change-address $addr2 \
--protocol-params-file protocol.json \
--out-file unlock.raw


cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in $TXIN \
--tx-in $TXINa \
--tx-out $addr2+"2000000 + $amountCBLP" \
--tx-out $TR+"2000000 + $amountUSD" \
--tx-out-inline-datum-file script/tTDatum.json \
--change-address $addr2 \
--protocol-params-file protocol.json \
--out-file lock.raw


cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in $TXINx \
--tx-in $TXINy \
--tx-out $addr2+" 1000000000 + $amountCBLP" \
--tx-out $SCR+"10000000 + $amountUSD" \
--tx-out-inline-datum-file datum/testTreasDatum.json \
--change-address $addr1 \
--protocol-params-file protocol.json \
--out-file tr.raw


cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in $TXINx \
--tx-in $TXINy \
--read-only-tx-in-reference $TXREF \
--tx-in $TXIN2 \
--tx-in-script-file ../output/treasuryXP.plutus \
--tx-in-inline-datum-present \
--tx-in-redeemer-file script/wR.json \
--tx-in-collateral $TXINy \
--tx-out $addr2+"2000000 + $cblpB + 1 $Lpolicy.$TOKENNAME2" \
--tx-out $LN+"100000001 + $cblpL" \
--tx-out-inline-datum-file ../output/lnXP.json \
--tx-out $TR+"2000000 + $leftUSD + 1 $pol1.$statetoken" \
--tx-out-inline-datum-file ../output/trXP.json \
--mint "1 $Lpolicy.$TOKENNAME2" \
--mint-script-file ../output/mint1.plutus \
--mint-redeemer-file script/wR.json \
--change-address $addr2 \
--protocol-params-file protocol.json \
--out-file unlock.raw

--mint "1 $Lpolicy.$TOKENNAME2" \
--mint-script-file ../output/mint1.plutus \
--mint-redeemer-file final/wR.json \

--calculate-plutus-script-cost cost4.json








--tx-out $addr2+"100000000 + 1000000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.7443424c50 +100000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.74555344" \

cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in f6c77c43625a124f7537e7181fc06dae2655f3311c4f94fe683492e474aacc4f#0 \
--tx-in f6c77c43625a124f7537e7181fc06dae2655f3311c4f94fe683492e474aacc4f#2 \
--tx-in $TRIN \
--tx-in-script-file output1/treasuryXP.plutus \
--tx-in-inline-datum-present \
--tx-in-redeemer-file script/xR.json \
--tx-in-collateral f6c77c43625a124f7537e7181fc06dae2655f3311c4f94fe683492e474aacc4f#0 \
--tx-out $TR2+"$trval" \
--tx-out-inline-datum-file ../output/treasuryDatum.json \
--tx-out $addr1+"$paramval" \
--tx-out-inline-datum-file ../output/paramDatum.json \
--tx-out addr_test1qrq7elwucxecdxqd9jk5dma2mx0rw08hqwvt0sqak68y7nu0mg8te7j2h77e2qf0xj7w4ktpexjvryaw38rf7x7f2phq46zlex+"5000000 + 10000000000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.7443424c50" \
--change-address $addr1 \
--protocol-params-file protocol.json \
--out-file unlock.raw

100000001 lovelace + 10000000000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.7443424c50

--tx-out $addr1+"3000000 lovelace + 1 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.706172616d746f6b656e" \
--tx-out-inline-datum-file final/paramXP.json \
$trval

cardano-cli transaction build \
--babbage-era \
$TEST \
--invalid-before 16194239 \
--tx-in $TXINl \
--tx-in $TXINm \
--read-only-tx-in-reference $paramhash \
--tx-in $trhash \
--tx-in-script-file ../output/treasuryXP.plutus \
--tx-in-inline-datum-present \
--tx-in-redeemer-file script/wR.json \
--tx-in-collateral $TXINm \
--tx-out $LN+"1000000000 + 1000000000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.7443424c50" \
--tx-out-inline-datum-file ../output/loanDatum.json \
--tx-out $addr2+"3000000 + 1000000000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.74555344 + 1 8dc44235b27cd1ab17cbacf0b66e4c445d692f5b9a670a9137e02158.a1f064ef94fa404d9fba7ab5b2c25f95eeea380b8c14cd7640e13ade02307c75 + 9979000000000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.7443424c50" \
--tx-out $TRnew+"3000000 + 1 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.7374617465746f6b656e + 998600000000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.74555344" \
--tx-out-inline-datum-file ../output/treasuryDatum.json \
--mint "1 8dc44235b27cd1ab17cbacf0b66e4c445d692f5b9a670a9137e02158.a1f064ef94fa404d9fba7ab5b2c25f95eeea380b8c14cd7640e13ade02307c75" \
--mint-script-file ../output/mint1.plutus \
--mint-redeemer-file script/wR.json \
--change-address $addr2 \
--protocol-params-file protocol.json \
--calculate-plutus-script-cost cost4.json
--out-file unlock.raw

--invalid-before 16194239 \

--mint "-1 $Lpolicy.$TOKENNAME2" \



cardano-cli transaction build \
--babbage-era \
$TEST \
--invalid-hereafter 16206561 \
--tx-in $TXINl \
--tx-in $TXINm \
--tx-in cf1db56e3dbfa750ea936ae7fcbff9d8a485db7b14474ed3c6e0fc8a9b6f916c#1 \
--read-only-tx-in-reference $paramhash \
--tx-in $trhash \
--tx-in-script-file ../output/treasuryXP.plutus \
--tx-in-inline-datum-present \
--tx-in-redeemer-file script/uR.json \
--tx-in $lnhash \
--tx-in-script-file ../output/loanXP2.plutus \
--tx-in-inline-datum-present \
--tx-in-redeemer-file script/wR.json \
--tx-in-collateral $TXINm \
--tx-out $addr2+"3000000 + 1000000000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.74555344 + 9980000000000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.7443424c50" \
--tx-out $TRnew+"$trval" \
--tx-out-inline-datum-file ../output/treasuryDatum.json \
--mint "-1 8dc44235b27cd1ab17cbacf0b66e4c445d692f5b9a670a9137e02158.a1f064ef94fa404d9fba7ab5b2c25f95eeea380b8c14cd7640e13ade02307c75" \
--mint-script-file ../output/mint1.plutus \
--mint-redeemer-file script/wR.json \
--change-address $addr2 \
--protocol-params-file protocol.json \
--out-file mint.raw









cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in $TXINl \
--tx-in $TXINm \
--read-only-tx-in-reference $paramhash \
--tx-in $lnhash \
--tx-in-script-file output2/treasuryXP.plutus \
--tx-in-inline-datum-present \
--tx-in-redeemer-file script/uR.json \
--tx-in-collateral $TXINm \
--tx-out $addr2+"100000000 + 9980000000000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.7443424c50" \
--tx-out $addr2+"3000000 + 100000000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.74555344" \
--tx-out $TR2+"300000000" \
--tx-out-inline-datum-file ../output/treasuryDatum.json \
--mint "$mintval" \
--mint-script-file ../output/mint1.plutus \
--mint-redeemer-file script/wR.json \
--change-address $addr2 \
--protocol-params-file protocol.json \
--out-file unlock.raw



cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in fdf693b2782b74c3db067022e1f6c832469864834894da883e8e5d019dac13bf#2 \
--tx-in fdf693b2782b74c3db067022e1f6c832469864834894da883e8e5d019dac13bf#0 \
--tx-in fdf693b2782b74c3db067022e1f6c832469864834894da883e8e5d019dac13bf#1 \
--tx-in-script-file output4/treasuryXP.plutus \
--tx-in-inline-datum-present \
--tx-in-redeemer-file script/uR.json \
--tx-in-collateral fdf693b2782b74c3db067022e1f6c832469864834894da883e8e5d019dac13bf#0 \
--tx-out $TRnew+"3000000 lovelace + 1 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.7374617465746f6b656e + 999600000000 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.74555344" \
--tx-out-inline-datum-file ../output/treasuryDatum.json \
--tx-out $addr1+"3000000 lovelace + 1 16b1a90ae98adfc92bd40fed1caf5869ba0aa08b43a8d21c96cb5016.706172616d746f6b656e" \
--tx-out-inline-datum-file ../output/paramDatum.json \
--change-address $addr1 \
--protocol-params-file protocol.json \
--out-file unlock.raw











cardano-cli transaction build \
--babbage-era \
$TEST \
--tx-in $TXINy \
--tx-out $LN2+"10000000" \
--tx-out-inline-datum-file ../output/lnXP.json \
--change-address $addr2 \
--protocol-params-file protocol.json \
--out-file unlock.raw