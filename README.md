# EtherDotBlue-RV-IOHK-2
A DeFi Stake Pool POC

## Requirements

* User will stake some Lovelance amount
* User will receive a stake pool token 'T' in exchange that will represent the locked value in a 1:1 ratio
* Pool token 'T' will be minted at the time of the staking
* User will specify the staking period in 'Slots', 30, 60 or 90
* The Stake Pool will set a different rate depending on the locked period
* The Stake Pool will have a minimum staking amount of 1000
* The staked funds will be locked until the period of staking completes
* User will redeem his funds using the pool token 'T'
* Pool token 'P' will be burned at the time of the redeem
* Multiple users can stake indepentendly
* Write a development journal with details about the experience during development

## Development Journal

Work in progress!

## How to build
```sh
cabal update
cabal build
```

## How to run

