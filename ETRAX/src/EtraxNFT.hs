{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}


module EtraxNFT where 

import           PlutusTx                   (compile, unstableMakeIsData)
import           PlutusTx.Prelude           (Bool (..), Eq ((==)), traceIfFalse, ($), (&&))
import           Plutus.V1.Ledger.Value     (flattenValue)
import           Plutus.V2.Ledger.Api       (BuiltinData,
                                             MintingPolicy,
                                             PubKeyHash,
                                             ScriptContext (scriptContextTxInfo),
                                             TxInfo (txInfoMint),
                                             mkMintingPolicyScript)
import           Prelude              (IO)
import           Mappers          (wrapPolicy)
import           Serialization    (writePolicyToFile, writeDataToFile) 
import Plutus.V2.Ledger.Contexts (txSignedBy)

--THE ON-CHAIN CODE

data EtraxParams = EtraxParams {
    pkh :: PubKeyHash,
    isMinting :: Bool
}

unstableMakeIsData ''EtraxParams

{-# INLINABLE etraxNFT #-}
etraxNFT :: EtraxParams -> ScriptContext -> Bool
etraxNFT params sContext = if isMinting params then forging else burning        
    where
        forging = traceIfFalse "PubKeyHash provided doesn't match!" isPubKey &&
                  traceIfFalse "Hold your horses.... Only 1 token at a time!" checkMintedAmount

        burning = traceIfFalse "PubKeyHash provided doesn't match!" isPubKey &&
                  traceIfFalse "Only burning one, nothing more, nothing less!" checkBurnedAmount 

        info :: TxInfo
        info = scriptContextTxInfo sContext
        
        isPubKey :: Bool
        isPubKey = txSignedBy info $ pkh params

        checkMintedAmount :: Bool
        checkMintedAmount = case Plutus.V1.Ledger.Value.flattenValue (txInfoMint info) of
            [(_, _, amt)] -> amt == 1
            _             -> False

        checkBurnedAmount :: Bool
        checkBurnedAmount = case Plutus.V1.Ledger.Value.flattenValue (txInfoMint info) of
            [(_, _, amt)] -> amt == -1
            _             -> False


{-# INLINABLE wrappedEtraxNFTPolicy #-}
wrappedEtraxNFTPolicy ::BuiltinData  -> BuiltinData -> ()
wrappedEtraxNFTPolicy = wrapPolicy etraxNFT
    
etraxNFTPolicy :: MintingPolicy
etraxNFTPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrappedEtraxNFTPolicy ||])
------------------------------------------------------------------------------------------

{- Serialised Scripts and Values -}

paramPkh :: PubKeyHash
paramPkh  = "32af4aba093e4d53e4e5f0dc6cd5d23703d89b852e7d54babdb48b81"

saveEtraxNFTPolicy :: IO ()
saveEtraxNFTPolicy =  writePolicyToFile "./testnet/etraNFT.policy" etraxNFTPolicy

saveRedeemerForging :: IO ()
saveRedeemerForging = writeDataToFile "./testnet/Forge.json" $ EtraxParams paramPkh True

saveRedeemerBurning :: IO ()
saveRedeemerBurning = writeDataToFile "./testnet/Burn.json" $ EtraxParams paramPkh True

saveAll :: IO ()
saveAll = do
            
            saveEtraxNFTPolicy
            saveRedeemerForging
            saveRedeemerBurning