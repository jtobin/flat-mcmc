#!/bin/bash
DEST_DIR=../data/output

# consider passing these args to the script
nepochs=1000
burnIn=200
inits=../data/input/inits-2d.dat

NOW=$(date +"%Y-%m-%d-%H%M")

./Himmelblau_Flat $nepochs $burnIn $inits \
    +RTS -N -qg -s \
    >  $DEST_DIR/himmelblau_"$nepochs"_"$NOW".dat \
    2> $DEST_DIR/himmelblau_"$nepochs"_"$NOW".rts

