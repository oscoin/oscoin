#!/bin/bash

set -euo pipefail

function format() {
  echo "Formatting $1"
  stack exec -- stylish-haskell --inplace "$1" || echo "Error formatting: $1" && true
}

export -f format

git ls-files | grep '\.l\?hs$' | xargs -P8 -n1 bash -c 'format "$@"' _

exit $(git ls-files --modified | wc -l)
