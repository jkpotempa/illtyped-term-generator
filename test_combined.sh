#!/usr/bin/bash

currentProgram=0
# ocamlopt -O2 helpers.ml types.ml tracemem.ml prettyprinter.ml generation.ml -o generator
while true; do
    timeout 2 ./generator
    if ghc -o illtyped_haskell illtyped.hs | grep -q "\[2 of 2\]"; then # [2 of 2] is the string denoting successful compilation in GHC
        cp expr_log.txt bugs/expr_log_bug_haskell$currentProgram.txt
        cp illtyped.hs bugs/illtyped_bug_haskell$currentProgram.hs
        # break;
    fi
    ocamlc -o illtyped_ocaml illtyped.ml
    if [ $? -eq 0 ]; then # status code 0 denotes successful compilation
        if [ -s illtyped.ml ]; then # only copy the log and program files if the program is not empty
            cp expr_log.txt bugs/expr_log_bug_ocaml$currentProgram.txt
            cp illtyped.ml bugs/illtyped_bug_ocaml$currentProgram.ml
        fi
    fi
    currentProgram=$((currentProgram + 1))
done