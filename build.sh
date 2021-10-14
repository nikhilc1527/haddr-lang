#!/bin/sh

# cabal clean
cabal build
echo "----------------------------------------------------\n"
./haddr-lang run test/test.haddr
