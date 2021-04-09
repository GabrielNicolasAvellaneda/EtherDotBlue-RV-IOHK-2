{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields  #-}
-- NOTE: NoImplicitPrelude should be set when using PlutusTx.Prelude to not clash with default Haskell's Prelude
{-# LANGUAGE NoImplicitPrelude  #-}

module Main where

import PlutusTx.Prelude

hello :: String
hello = "Hello, I'm a DeFi Stake Pool baby ;)"

main :: IO ()
main = do
    putStrLn hello