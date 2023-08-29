utxoin="6c887bcc55981818be601f271b1c2bfdd00e1f5fc62acb4d7c70e3c64d48c6fe#1"
policyid=$(cat etraxNFT.pid)
address=$(cat ../../../Addresses/myAddr1.addr)
output="1163700"
tokenamount="1"
tokenname=$(echo -n "EtraxNFT" | xxd -ps | tr -d '\n')
collateral="6c887bcc55981818be601f271b1c2bfdd00e1f5fc62acb4d7c70e3c64d48c6fe#1"
signerPKH="32af4aba093e4d53e4e5f0dc6cd5d23703d89b852e7d54babdb48b81"
PREVIEW="--testnet-magic 2"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $address+$output+"$tokenamount $policyid.$tokenname" \
  --metadata-json-file ../src/metaDataNFT.json \
  --change-address $address \
  --mint "$tokenamount $policyid.$tokenname" \
  --mint-script-file etraxNFT.policy \
  --mint-redeemer-file Forge.json \
  --protocol-params-file protocol.params \
  --out-file mintEtraxNFT.body

cardano-cli transaction sign \
    --tx-body-file mintEtraxNFT.body \
    --signing-key-file ../../../Addresses/payment1.skey \
    $PREVIEW \
    --out-file mintEtraxNFT.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file mintEtraxNFT.signed