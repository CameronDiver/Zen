#!/bin/bash

curl -sL https://raw.github.com/jaspervdj/stylish-haskell/master/scripts/latest.sh | sh -s . -i -r src app test

if ! git diff-index --quiet HEAD --; then
  exit 1
fi
