#!/bin/bash

set -ex

killall plutus-pab || true

rm -rf pab-core.db* 

cabal exec -- plutus-pab --config plutus-pab.yaml migrate pab-core.db

cabal exec -- plutus-pab --config plutus-pab.yaml contracts install --path "$(cabal exec -- which plutus-game)"

cabal exec -- plutus-pab all-servers --config plutus-pab.yaml
