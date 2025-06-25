#!/usr/bin/bash

currentProgram=0
ocamlopt -O2 helpers.ml types.ml tracemem.ml prettyprinter.ml generation.ml
while true; do
    timeout 2 ./a.out 
    if ghc illtyped.hs | grep -q "\[2 of 2\]"; then # [2 of 2] is the string denoting successful compilation in GHC
        cp expr_log.txt expr_log_bug$currentProgram.txt
        cp illtyped.hs illtyped_bug$currentProgram.hs
        currentProgram=$((currentProgram + 1))
    fi
done