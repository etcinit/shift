#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

docker build -t shift .

# If this is not a pull request, update the branch's docker tag.
if [ $TRAVIS_PULL_REQUEST = 'false' ]; then
  docker tag shift quay.io/etcinit/shift:${TRAVIS_BRANCH/\//-} \
    && docker push quay.io/etcinit/shift:${TRAVIS_BRANCH/\//-};

  # If this commit has a tag, use on the registry too.
  if ! test -z $TRAVIS_TAG; then
    docker tag shift quay.io/etcinit/shift:${TRAVIS_TAG} \
      && docker push quay.io/etcinit/shift:${TRAVIS_TAG};
  fi
fi
