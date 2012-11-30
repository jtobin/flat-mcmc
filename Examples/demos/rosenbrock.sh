#!/bin/bash
DEST_DIR=../data/output

# consider passing these args to the script
nepochs=1000
inits=../data/input/inits-2d.dat

NOW=$(date +"%Y-%m-%d-%H%M")

./Rosenbrock_Flat $nepochs $inits \
    +RTS -N -qg -s \
    >  $DEST_DIR/rosenbrock_"$nepochs"_"$NOW".dat \
    2> $DEST_DIR/rosenbrock_"$nepochs"_"$NOW".rts

