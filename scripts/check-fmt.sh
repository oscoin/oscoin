#!/bin/bash

set -euo pipefail

stack exec -- stylish-haskell **/**.hs --inplace
modified=$(git diff --name-only)
count=$(echo -n "$modified" | wc -l)

if [[ $count -ne 0 ]]; then
  printf "Found %d misformatted files:\n%s" $count $modified
fi

exit $count
