#!/bin/bash

echo Current version: `grep "^Version" *.cabal | sed "s/^Version:\s*//"`
echo -n New version:\ 
read ver

out=`sed "s/^\(Version:\s*\)[0-9.]*/\1$ver/" *.cabal`
echo "$out" > *.cabal
