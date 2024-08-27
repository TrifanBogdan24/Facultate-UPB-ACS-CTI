#!/bin/bash

for file in *.hs ; do
    ghc "$file"
done
