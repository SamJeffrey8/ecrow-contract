{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE NoImplicitPrelude             #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE TypeOperators                 #-}
{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module AuthNFTIssuer(issuerCS, endpoints, AuthNFTIssuerSchema) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map hiding (empty)
import           Data.Text            (Text, unpack)
import           Data.Monoid          (Last (..))
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (toBuiltinData)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import qualified Prelude
import           Text.Printf          (printf)
import           Data.Text.Prettyprint.Doc.Extras (PrettyShow (..))
import           Prelude              (Semigroup (..), Show (..), Eq)
import           Plutus.Contract       as Contract
import           Ledger.Value           as Value
import           Ledger                 hiding (mint, singleton)
import           Plutus.V1.Ledger.Value (Value (..), assetClass, assetClassValueOf)
import           PlutusTx.Builtins.Class
import           Wallet.Emulator.Wallet (Wallet, walletPubKey)


{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

issuerCS :: PubKeyHash -> CurrencySymbol
issuerCS = scriptCurrencySymbol . policy

data Arg = Arg
    { time    :: String
    , cWallet :: Wallet 
    , pWallet :: Wallet
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


type AuthNFTIssuerSchema =
                  Endpoint "mint" Arg
              .\/ Endpoint "inspect" String  {-Argument can be just () instead of String. Not done due to want of time-}
              .\/ Endpoint "logWalletNftTokenName" ()

book :: forall w s e. AsContractError e => Arg -> Contract w s e ()
book arg = do
    pkh <- pubKeyHash <$> ownPubKey
    let reqPk = (pubKeyHash . walletPubKey) $ cWallet arg
    let timeDate = time arg
        val     = Value.singleton (issuerCS pkh) (TokenName  $ stringToBuiltinByteString  timeDate) 1
        lookups = Constraints.mintingPolicy $ policy pkh
        tx      = Constraints.mustMintValue val <>
                  Constraints.mustPayToPubKey reqPk val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "minted %s" (show val)



mint' :: Promise () AuthNFTIssuerSchema Text ()
mint' = endpoint @"mint" book



endpoints :: AsContractError e => Contract () AuthNFTIssuerSchema Text e
endpoints = do
    logInfo @String "Waiting for request."
    selectList [mint'] >>  endpoints

mkSchemaDefinitions ''AuthNFTIssuerSchema
mkKnownCurrencies []
