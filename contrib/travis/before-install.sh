#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

mkdir -p ~/.local/bin

export PATH=$HOME/.local/bin:$PATH

curl -L https://www.stackage.org/stack/linux-x86_64 \
  | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
curl -L https://github.com/aktau/github-release/releases/download/v0.6.2/linux-amd64-github-release.tar.bz2 | tar -xj --wildcards --strip-components=3 -C ~/.local/bin '*/github-release'

stack setup
stack install hscolour

docker login -e="." -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD" quay.io

mkdir dist
