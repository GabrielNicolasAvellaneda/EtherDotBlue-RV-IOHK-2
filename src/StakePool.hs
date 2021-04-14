{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module StakePool
  (
  )
where

import Control.Monad
import Data.Text hiding (head)
import Ledger
import Ledger.Ada as Ada
import Ledger.Constraints
import qualified Ledger.Scripts as Scripts
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract
import Plutus.Contract hiding (when)
import qualified PlutusTx
import PlutusTx.Prelude
import Text.Printf (printf)
import           Data.Map             as Map

data StakePoolParams = StakePoolParams
  { minAmount :: !Integer
  }

defaultParams :: StakePoolParams
defaultParams = StakePoolParams {minAmount = 5}

data StakePoolDatum = StakePoolDatum
  { 
    spdOwner   :: !PubKeyHash,
    spdAmount :: !Integer,
    spdPeriod :: !Slot
  }
  deriving (Show)

PlutusTx.unstableMakeIsData ''StakePoolDatum
PlutusTx.makeLift ''StakePoolDatum

data StakePoolRedeemer = StakePoolRedeemer
  { sprAmount :: !Integer
  }
  deriving (Show)

PlutusTx.unstableMakeIsData ''StakePoolRedeemer
PlutusTx.makeLift ''StakePoolRedeemer

data StakeParams = StakeParams
  { spPeriod :: !Slot,
    spAmount :: !Integer
  }
  deriving (Generic, Show, ToSchema, ToJSON, FromJSON)

data RedeemParams = RedeemParams
  { rpAmount :: !Integer
  }
  deriving (Generic, Show, ToSchema, ToJSON, FromJSON)

{-# INLINEABLE mkStakePoolValidator #-}
mkStakePoolValidator :: StakePoolDatum -> StakePoolRedeemer -> ValidatorCtx -> Bool
mkStakePoolValidator _ _ _ = True

data StakePool

instance Scripts.ScriptType StakePool where
  type RedeemerType StakePool = StakePoolRedeemer
  type DatumType StakePool = StakePoolDatum

inst :: Scripts.ScriptInstance StakePool
inst =
  Scripts.validator @StakePool
    $$(PlutusTx.compile [||mkStakePoolValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @StakePoolDatum @StakePoolRedeemer

validator :: Validator
validator = Scripts.validatorScript inst

stakePoolHash :: ValidatorHash
stakePoolHash = Scripts.validatorHash validator

stakePoolAddr :: Address
stakePoolAddr = ScriptAddress stakePoolHash

stake :: (HasBlockchainActions s) => StakeParams -> Contract w s Text ()
stake StakeParams {..} = do
  pkh <- pubKeyHash <$> ownPubKey
  let min = minAmount defaultParams
  when (spAmount < min) $
    throwError $ pack $ printf "the minimum staking amount is %d lovelace" min

  let v = Ada.lovelaceValueOf spAmount
      d =
        StakePoolDatum
          { spdOwner = pkh,
            spdAmount = spAmount,
            spdPeriod = spPeriod
          }
      tx = mustPayToTheScript d v
  ledgerTx <- submitTxConstraints inst tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ "staked " <> show spAmount <> " lovelace"

redeem :: (HasBlockchainActions s) => RedeemParams -> Contract w s Text ()
redeem RedeemParams {..} = do
  pkh <- pubKeyHash <$> ownPubKey
  utxos <- findStakeTxs pkh rpAmount
  case utxos of
    (oref, o):xs -> do
      logInfo @String $ printf "found utxo with %s" (show oref)
      let r = StakePoolRedeemer {sprAmount = rpAmount}
          unspentOutputs = (Map.singleton oref o)
          tx = collectFromScript unspentOutputs  r
      ledgerTx <- submitTxConstraintsSpending inst unspentOutputs tx
      void $ awaitTxConfirmed $ txId ledgerTx
      logInfo @String $ printf "redeemed %d lovelances" rpAmount
    _ -> logError @String $ printf "cannot find any stake for the amount of %d lovelace" rpAmount


findStakeTxs :: HasBlockchainActions s
            => PubKeyHash ->Integer
            -> Contract w s Text [(TxOutRef, TxOutTx)]
findStakeTxs pkh amount = do
    -- TODO: Just find uxtos for the current wallet.
    utxos <- utxoAt stakePoolAddr
    logInfo @String $ show utxos
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Ada.fromValue (txOutValue $ txOutTxOut o) == lovelaceOf amount && True
              
              
             ]
    case xs of
        [(oref, o)] -> case txOutType $ txOutTxOut o of
            PayToPubKey   -> throwError "unexpected out type"
            PayToScript h -> case Map.lookup h $ txData $ txOutTxTx o of
                Nothing        -> throwError "datum not found"
                Just (Datum e) -> case PlutusTx.fromData e of
                    Nothing -> throwError "datum has wrong type"
                    Just d@StakePoolDatum{..}
                        | spdOwner == pkh -> return [(oref, o)]
                        | otherwise -> throwError "can not find stake"
        _           -> throwError "stake utxo not found"   


type StakePoolSchema =
  BlockchainActions
    .\/ Endpoint "stake" StakeParams
    .\/ Endpoint "redeem" RedeemParams

endpoints :: Contract () StakePoolSchema Text ()
endpoints = (stake' `select` redeem') >> endpoints
  where
    stake' = endpoint @"stake" >>= stake
    redeem' = endpoint @"redeem" >>= redeem

mkSchemaDefinitions ''StakePoolSchema
mkKnownCurrencies []
