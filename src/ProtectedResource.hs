{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module ProtectedResource(endpoints, ProtectedResourceSchema) where

import           Control.Lens           (view)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import qualified Data.Map               as Map hiding (empty)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           AuthNFTIssuer      as I hiding (endpoints)
import           Plutus.Contract
import qualified PlutusTx
import qualified Prelude
import           PlutusTx.Builtins.Class
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Playground.Contract    (ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String, Semigroup (..) )
import           Wallet.Emulator.Wallet (Wallet, walletPubKey)

data CheckArg = CheckArg
    { time         :: !String 
    , issuerWallet :: !Wallet
    , clientWallet :: !Wallet
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
    
type ProtectedResourceSchema =
                  Endpoint "checkAccess" CheckArg

checkAccess :: forall w s e. AsContractError e => CheckArg -> Contract w s e ()
checkAccess arg = do
    pkh <- pubKeyHash <$> ownPubKey
    let iw = issuerWallet arg
    let timeDate = time arg
        cw = clientWallet arg
    os  <- map snd . Map.toList <$> utxosAt (pubKeyAddress $ walletPubKey cw)
    let nftVal = mconcat [view ciTxOutValue o | o <- os, nf (view ciTxOutValue o) iw cw]
        qty = assetClassValueOf (nftVal) (AssetClass (I.issuerCS (pubKeyHash $ walletPubKey iw), TokenName  $ stringToBuiltinByteString  timeDate))
    logInfo @String $ "Total value at client wallet" <> (show nftVal)
    logInfo @String $ "LOGIN ACCESS - " ++ (if qty == 0 then "DENIED" else "GRANTED")
    where
      nf val iw cw = assetClassValueOf (val) (AssetClass (I.issuerCS (pubKeyHash $ walletPubKey iw), (TokenName  $ stringToBuiltinByteString $ time arg))) == 1

checkAccess' :: Promise () ProtectedResourceSchema Text ()
checkAccess' = endpoint @"checkAccess" checkAccess

endpoints :: AsContractError e => Contract () ProtectedResourceSchema Text e
endpoints = do
    logInfo @String "Waiting for request."
    selectList [checkAccess'] >>  endpoints

mkSchemaDefinitions ''ProtectedResourceSchema
mkKnownCurrencies []
