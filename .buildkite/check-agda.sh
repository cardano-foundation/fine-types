#!/usr/bin/env bash

set -euo pipefail

cd .buildkite/check-agda

echo "### Checking AGDA_DIR"
echo AGDA_DIR=$AGDA_DIR

echo "### Checking Agda"
agda --version
agda \
    --no-default-libraries \
    --local-interfaces \
    Everything.agda && rm Everything.agdai

echo "### Checking agda2hs"
agda2hs --version
agda2hs \
    --no-default-libraries \
    --local-interfaces \
    Everything.agda && rm Everything.hs Everything.agdai
