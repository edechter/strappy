#!/bin/bash

## user should be in ROOT directory of Strappy project
cabal configure --enable-tests
cabal build
cabal test --show-details=always



