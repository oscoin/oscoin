#!/usr/bin/env bash

set -euo pipefail

shopt -s globstar

base=$(mktemp -d "/tmp/oscoin-base.XXXXX")
for f in **/**.{hs,sh}; do
  path=$base/$(dirname $f)
  mkdir -p $path
  cp $f $path
done

formatted=$(mktemp -d "/tmp/oscoin-formatted.XXXXX")
cp -R $base/* $formatted

# Haskell
stack exec -- stylish-haskell $formatted/**/**.hs --inplace

# Shell scripts
# -s    simplify the code
# -i 2  indent with two spaces
# -bn   binary ops like && and | may start a line
# -sr   redirect operators will be followed by a space
# -w    write result to file instead of stdout
shfmt -w -s -i 2 -bn -sr -l .

diff -rq $base $formatted
