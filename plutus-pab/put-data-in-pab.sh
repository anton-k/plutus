#!/bin/bash

set -ex

killall plutus-pab || true

rm -rf pab-core.db* 

cabal run exe:plutus-pab -- --config plutus-pab.yaml migrate pab-core.db

cabal run exe:plutus-pab -- --config plutus-pab.yaml contracts install --path "$(cabal exec -- which plutus-game)"

cabal run exe:plutus-pab -- all-servers --config plutus-pab.yaml
