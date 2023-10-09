#!/usr/bin/env bash

set -euo pipefail

nix build --accept-flake-config .#fine-types:exe:fine-types
cd lib/cardano-ledger-specs/
fine_types="../../result/bin/fine-types"
echo "Checking fine types for Shelley"
$fine_types check -i Shelley.fine
echo "Checking fine types for Mary"
$fine_types check -i Mary.fine
echo "Checking fine types for Allegra"
$fine_types check -i Allegra.fine
echo "Checking fine types for Alonzo"
$fine_types check -i Alonzo.fine
echo "Checking fine types for Babbage"
$fine_types check -i Babbage.fine