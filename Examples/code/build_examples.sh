#!/bin/bash
EXAMPLES_DIR=/Users/jtobin/projects/flat-mcmc/Examples/code
DEST_DIR=/Users/jtobin/projects/flat-mcmc/Examples/demos
GHC_ARGS="-O2 +RTS -fllvm -threaded -rtsopts"

cd $EXAMPLES_DIR

# Compile examples
for file in *hs
do
    ghc $file -O2 -fllvm -threaded -rtsopts
done

# Strip the binaries and move to destination
BINS=$(ls *[^.hs])

for bin in $BINS
do
    strip $bin
    mv $bin $DEST_DIR
done

# Clean up
rm *.hi *.o

