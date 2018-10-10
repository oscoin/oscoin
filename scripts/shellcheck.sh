#!/usr/bin/env bash

set -euo pipefail

shopt -s globstar

docker run -w /workspace -v "$(pwd):/workspace" koalaman/shellcheck:stable **/**.sh
