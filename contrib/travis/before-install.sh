#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

mkdir -p ~/.local/bin

export PATH=$HOME/.local/bin:$PATH

curl -L https://www.stackage.org/stack/linux-x86_64 \
  | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

stack docker pull
stack install hscolour

docker login -e="." -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD" quay.io
