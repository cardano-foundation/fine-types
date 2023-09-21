#!/usr/bin/env bash

set -euo pipefail

nix build --accept-flake-config .#fine-types:exe:fine-types
cd lib/cardano-ledger-specs/
fine_types="../../result/bin/fine-types"
$fine_types check -i Shelley.fine
$fine_types check -i Allegra.fine
$fine_types check -i Mary.fine
$fine_types check -i Alonzo.fine
$fine_types check -i Babbage.fine