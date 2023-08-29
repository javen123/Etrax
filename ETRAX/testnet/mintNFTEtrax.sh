utxoin1="3b3c87bc71d72d169af15bf7dd5f1793bc9a40ab6eadcf2d3b3cc66e8ae4de6a#0"
policyid=$(cat etraxNFT.pid)
address=""
tokenamount="1"
tokenname=$(echo -n "EtraxNFT" | xxd -ps | tr -d '\n')
collateral=""
signerPKH="32af4aba093e4d53e4e5f0dc6cd5d23703d89b852e7d54babdb48b81"
PREVIEW="--testnet-magic 2"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin2 \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $address+"$tokenamount $policyid.$tokenname" \
  --change-address $address \
  --mint "$tokenamount $policyid.$tokenname" \
  --mint-script-file etraxNFT.policy \
  --mint-redeemer-file Forge.json \
  --protocol-params-file protocol.params \
  --out-file mintEtraxNFT.body

cardano-cli transaction sign \
    --tx-body-file mintEtraxNFT.body \
    --signing-key-file ../../../../Addresses/payment1.skey \
    $PREVIEW \
    --out-file mintEtraxNFT.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file mintEtraxNFT.signed