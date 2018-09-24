#!/bin/bash

set -euo pipefail

function load-cache() {
    bucket="gs://oscoin-build-cache"
    key="stack-work-$(cat stack.yaml | sha256sum | cut -d ' ' -f 1)".tar.gz
    gsutil -m cp "$bucket/v1/stack.tar.gz" "$bucket/$key" . || true
    for f in stack.tar.gz $key; do
      if [[ -e $f ]]; then
        tar xzf $f
      fi
    done
}

function save-cache() {
    bucket="gs://oscoin-build-cache"
    key="stack-work-$(cat stack.yaml | sha256sum | cut -d ' ' -f 1)".tar.gz
    if ! gsutil ls "$bucket/$key"; then
      tar czf $key .stack-work
      gsutil -m cp $key "$bucket/$key" || true
    fi

    rm -rf .stack/indices/Hackage/00-index.tar*
    tar czf stack.tar.gz .stack
    gsutil -m cp stack.tar.gz "$bucket/v1/stack.tar.gz" || true
}
