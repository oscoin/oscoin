#!/bin/bash

set -euo pipefail

if ! which shfmt > /dev/null; then
  curl -s -L -o /usr/bin/shfmt "https://github.com/mvdan/sh/releases/download/v2.5.1/shfmt_v2.5.1_linux_amd64"
  chmod +x /usr/bin/shfmt
fi

# -s    simplify the code
# -i 2  indent with two spaces
# -bn   binary ops like && and | may start a line
# -ci   switch cases will be indented
# -sr   redirect operators will be followed by a space
# -d    display diffs and exit with non-zero status code
#       when formatting differs
shfmt -s -i 2 -bn -ci -sr -d scripts/
