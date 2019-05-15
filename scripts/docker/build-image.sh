#!/bin/bash

set -euo pipefail

bin=$(basename "$1")
img="eu.gcr.io/$PROJECT_ID/$bin"
dir=$(mktemp -d "/tmp/oscoin.XXXXXX")
cp "$1" "$dir"
cp -r data "$dir"
pushd "$dir"

cat > Dockerfile << EOF
FROM eu.gcr.io/$PROJECT_ID/haskell-scratch:integer-gmp
COPY $bin /bin/$bin
COPY data /data/data/
ENV oscoin_datadir=/data
ENTRYPOINT ["/bin/$bin"]
EOF

branch_name_tag=$(echo "$BRANCH_NAME" | tr "/" "-")

docker build --label "version=$COMMIT_SHA" \
  --tag "$img:$branch_name_tag" \
  --tag "$img:$COMMIT_SHA" .

if [[ $BRANCH_NAME == "master" ]]; then
  docker tag "$img:$BRANCH_NAME" "$img:latest"
fi

popd
