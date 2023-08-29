{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE OverloadedStrings   #-}  --Enable passing strings as other character formats, like bytestring.

module EtraxStates where

--PlutusTx 
import                  PlutusTx                       (BuiltinData, compile, unstableMakeIsData, makeIsDataIndexed)
import                  PlutusTx.Prelude               (traceIfFalse, otherwise, (==), Bool (..), Integer, ($), (>))
import                  Plutus.V1.Ledger.Value      as PlutusV1
import                  Plutus.V1.Ledger.Interval      (contains, to) 
import                  Plutus.V2.Ledger.Api        as PlutusV2
import                  Plutus.V2.Ledger.Contexts      (txSignedBy, valueSpent)
--Serialization
import                  Mappers                        (wrapValidator)
import                  Serialization                  (writeValidatorToFile, writeDataToFile)
import                  Prelude                         (IO)

--THE ON-CHAIN CODE
newtype DatumStates = State Integer
unstableMakeIsData ''DatumStates

data ActionsRedeemer = Owner PlutusV2.PubKeyHash | Address Text | NFT Text
unstableMakeIsData '' ActionsRedeemer

{-# INLINABLE conditionator #-}
conditionator :: DatumStates -> ScriptContext -> Bool
conditionator datum redeemer sContext = case redeemer of
                                         Owner   -> traceIfFalse    "Not signed properly!"  signedByOwner
                                         Address -> traceIfFalse    "Incorrect address. You can only send to burn script address or same address for state change" isAddressCorrect                                         
                                         NFT     -> traceIfFalse    "Price is not covered"  priceIsCovered
    where

        info :: TxInfo
        info = scriptContextTxInfo sContext

        signedByOwner :: Bool
        signedByOwner = txSignedBy info $ owner Owner

        isAddressCorrect :: Bool
        isAddressCorrect = contains ()

        priceIsCovered :: Bool
        priceIsCovered =  assetClassValueOf (valueSpent info)  (AssetClass (adaSymbol,adaToken)) > price datum

        


mappedCommonConditions :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mappedCommonConditions = wrapValidator conditionator

conditionsValidator :: Validator
conditionsValidator =  PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mappedCommonConditions ||])

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

{- Serialised Scripts and Values -}

saveConditionsValidator :: IO ()
saveConditionsValidator =  writeValidatorToFile "./testnet/conditionator.plutus" conditionsValidator

saveUnit :: IO ()
saveUnit = writeDataToFile "./testnet/unit.json" ()

saveDatum :: IO ()
saveDatum  = writeDataToFile "./testnet/datum.json" (Conditions "32af4aba093e4d53e4e5f0dc6cd5d23703d89b852e7d54babdb48b81" 1691774814000 50)

saveRedeemerOwner :: IO ()
saveRedeemerOwner = writeDataToFile "./testnet/redeemOwner.json" Owner

saveRedeemerTime :: IO ()
saveRedeemerTime = writeDataToFile "./testnet/redeemTime.json" Time

saveRedeemerPrice :: IO ()
saveRedeemerPrice = writeDataToFile "./testnet/redeemPrice.json" Price

saveAll :: IO ()
saveAll = do
            saveConditionsValidator
            saveUnit
            saveDatum
            saveRedeemerOwner
            saveRedeemerPrice
            saveRedeemerTime
