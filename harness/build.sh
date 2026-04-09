#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
elm make Main.elm --output=elm.js
echo "Built harness/elm.js"
