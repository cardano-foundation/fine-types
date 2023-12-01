#!/usr/bin/env bash

set -euo pipefail

cd .buildkite/check-agda

echo "### Checking Agda"
agda \
    --no-default-libraries \
    --local-interfaces \
    Everything.agda && rm Everything.agdai

echo "### Checking agda2hs"
agda2hs \
    --no-default-libraries \
    --local-interfaces \
    Everything.agda && rm Everything.hs Everything.agdai
