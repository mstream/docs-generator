#! /usr/bin/env bash

set -e

spago -x example/spago.dhall bundle-app \
  --main Example.Main \
  --to example/app.js
