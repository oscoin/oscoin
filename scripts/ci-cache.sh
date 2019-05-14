#!/usr/bin/env bash

set -euxo pipefail

deps_hash=$(cat package.yaml snapshot.yaml | sha256sum | cut -d' ' -f1)
bucket="gs://oscoin-build-cache/v6"
local_cache_archive="stack-root.tar.gz"
remote_cache_master="${bucket}/stack-root-master.tar.gz"
remote_cache_hashed="${bucket}/stack-root-${deps_hash}.tar.gz"

function load-cache() {
  if gsutil -q ls "$remote_cache_hashed"; then
    echo "Using hashed stack cache"
    gsutil cat "$remote_cache_hashed" | tar -xz
  elif gsutil -q ls "$remote_cache_master"; then
    echo "Using master stack cache"
    gsutil cat "$remote_cache_master" | tar -xz
  fi
}

function save-cache() {
  if ! gsutil -q ls "$remote_cache_hashed"; then
    echo "Uploading stack cache"
    # This file is not needed and unecessarily large
    rm -rf .stack/indices/Hackage/00-index.tar*
    tar czf $local_cache_archive .stack
    gsutil -m cp $local_cache_archive "$remote_cache_hashed"
  fi

  if [ "$BRANCH_NAME" = "master" ]; then
    echo "Setting master cache to current cache"
    gsutil -m cp "$remote_cache_hashed" "$remote_cache_master"
  fi
}
