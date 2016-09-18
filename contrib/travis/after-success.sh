#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

cp "$(stack path --local-bin)/shift" dist/

docker build -t shift .

# If this is not a pull request, update the branch's docker tag.
if [ $TRAVIS_PULL_REQUEST = 'false' ]; then
  docker tag shift quay.io/etcinit/shift:${TRAVIS_BRANCH/\//-} \
    && docker push quay.io/etcinit/shift:${TRAVIS_BRANCH/\//-};

  # If this commit has a tag, use on the registry too.
  if ! test -z $TRAVIS_TAG; then
    docker tag shift quay.io/etcinit/shift:${TRAVIS_TAG} \
      && docker push quay.io/etcinit/shift:${TRAVIS_TAG};

    github-release edit \
      --user etcinit \
      --repo shift \
      --tag ${TRAVIS_TAG} \
      --name ${TRAVIS_TAG} \
      --description "."

    github-release upload \
      --user etcinit \
      --repo shift \
      --tag ${TRAVIS_TAG} \
      --name "shift-linux-amd64" \
      --file dist/shift
  fi
fi
