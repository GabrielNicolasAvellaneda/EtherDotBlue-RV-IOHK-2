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
import Data.Text
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

data StakePoolParams = StakePoolParams
  { minAmount :: !Integer
  }

defaultParams :: StakePoolParams
defaultParams = StakePoolParams {minAmount = 5}

data StakePoolDatum = StakePoolDatum
  { spdAmount :: !Integer,
    spdPeriod :: !Slot
  }
  deriving (Show)

PlutusTx.unstableMakeIsData ''StakePoolDatum
PlutusTx.makeLift ''StakePoolDatum

data StakePoolRedeemer = StakePoolRedeemer
  {
  }
  deriving (Show)

PlutusTx.unstableMakeIsData ''StakePoolRedeemer
PlutusTx.makeLift ''StakePoolRedeemer

data StakeParams = StakeParams
  { spPeriod :: !Slot,
    spAmount :: !Integer
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
  let min = minAmount defaultParams
  when (spAmount < min) $
    throwError $ pack $ printf "the minimum staking amount is %d" min

  let v = Ada.lovelaceValueOf spAmount
      d =
        StakePoolDatum
          { spdAmount = spAmount,
            spdPeriod = spPeriod
          }
      tx = mustPayToTheScript d v
  ledgerTx <- submitTxConstraints inst tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ "staked " <> show spAmount <> " lovelace"

type StakePoolSchema =
  BlockchainActions
    .\/ Endpoint "stake" StakeParams

endpoints :: Contract () StakePoolSchema Text ()
endpoints = stake' >> endpoints
  where
    stake' = endpoint @"stake" >>= stake

mkSchemaDefinitions ''StakePoolSchema
mkKnownCurrencies []
