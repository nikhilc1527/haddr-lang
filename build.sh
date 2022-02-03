#!/bin/sh

cabal build
printf -- "----------------------------------------------------\n"
time -p ./haddr-lang run test/test.haddr
