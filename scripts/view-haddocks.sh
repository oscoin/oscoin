#!/usr/bin/env bash
set -e

base_url="https://storage.googleapis.com/oscoin-apidocs"
platform=$(uname)
if [ "$platform" == "Darwin" ]; then
  open="open"
else
  open="xdg-open"
fi

fetch=""
commit=""

while getopts "fc:" opt; do
  case $opt in
    f)
      fetch=true
      ;;
    c)
      commit="$OPTARG"
      ;;
    *)
      echo "Usage: view_haddocks [-f] [-c <commit>]"
      echo "If -f is given, 'git fetch origin' is run before doing anything"
      exit 1
      ;;
  esac
done

[[ $fetch == "" ]] || { git fetch origin; }
[[ $commit == "" ]] && { commit=$(git rev-parse origin/master); }

$open "${base_url}/${commit}/index.html"
