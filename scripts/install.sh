#!/bin/bash

rm -rf /tmp/testbuild &&
mkdir -p /tmp/testbuild &&
echo Creating temporary build environment in /tmp/testbuild &&
cp -r --preserve=timestamps . "/tmp/testbuild" &&

cd "/tmp/testbuild" &&
git reset --hard $1 > /dev/null &&
git clean -fd > /dev/null &&
cabal install 2>&1 | grep -v "^\(You are using a new version of LLVM that hasn't been tested yet\|We will try though\)" &&
echo
