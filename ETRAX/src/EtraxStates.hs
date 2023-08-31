{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE OverloadedStrings   #-}  --Enable passing strings as other character formats, like bytestring.

module EtraxStates where

--PlutusTx 
import                  PlutusTx                       (BuiltinData, compile, unstableMakeIsData, makeIsDataIndexed)
import                  PlutusTx.Prelude               (traceIfFalse, otherwise, (==),elem, Bool (..), Integer, ($), (>))
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

data EtraxRedeemer = EtraxRedeemer {owner :: PlutusV2.PubKeyHash,
                                    address :: BuiltinByteString
                                    }
unstableMakeIsData ''EtraxRedeemer

{-# INLINABLE conditionator #-}
conditionator :: DatumStates -> EtraxRedeemer -> ScriptContext -> Bool
conditionator datum redeemer sContext = case redeemer of
                                         owner   -> traceIfFalse    "Not signed properly!"  signedByOwner
                                         address -> traceIfFalse    "Incorrect address. You can only send to burn script address or same address for state change" isAddressCorrect                                         
                                         
    where

        info :: TxInfo
        info = scriptContextTxInfo sContext

        signedByOwner :: Bool
        signedByOwner = txSignedBy info $ owner redeemer

        isAddressCorrect :: Bool
        isAddressCorrect = elem $ address redeemer $ txInfoOutputs info
        
        isNFTPresent :: Bool
        isNFTPresent = contains "b2cf511eb261780ce184788d56bd6a38dabfb985a28487c7872edf8c" $ txInfoInputs info  

mappedCommonConditions :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mappedCommonConditions = wrapValidator conditionator

conditionsValidator :: Validator
conditionsValidator =  PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mappedCommonConditions ||])

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

{- Serialised Scripts and Values -}

saveConditionsValidator :: IO ()
saveConditionsValidator =  writeValidatorToFile "./testnet/conditionator.plutus" conditionsValidator

saveDatum :: IO ()
saveDatum  = writeDataToFile "./testnet/datum.json" (State 1)

saveRedeemerOwner :: IO ()
saveRedeemerOwner = writeDataToFile "./testnet/redeemOwner.json" (EtraxRedeemer "32af4aba093e4d53e4e5f0dc6cd5d23703d89b852e7d54babdb48b81","addr_test1qqe27j46pyly65lyuhcdcmx46gms8kyms5h86496hk6ghqgf87zy2chtpsk4y6hk6czt6xjh3ej8gxreystr6en7wqzssrl3lc")

saveAll :: IO ()
saveAll = do
            saveConditionsValidator
            
            
