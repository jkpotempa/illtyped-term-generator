#!/usr/bin/bash

# no tracking statistics, just bug finding

currentProgram=0
# ocamlopt -O2 helpers.ml types.ml tracemem.ml prettyprinter.ml generation.ml -o generator
while true; do
    timeout 2 ./generator -no-stat
    $HOME/.ghcup/bin/ghc-[VERSION] -o illtyped_haskell illtyped.hs
    if [ $? -eq 0 ]; then
        cp expr_log.txt bugs/expr_log_bug_haskell$currentProgram.txt
        cp illtyped.hs bugs/illtyped_bug_haskell$currentProgram.hs
        # break;
    fi
    _opam/bin/ocamlc -o illtyped_ocaml illtyped.ml
    if [ $? -eq 0 ]; then # status code 0 denotes successful compilation
        if [ -s illtyped.ml ]; then # only copy the log and program files if the program is not empty
            cp expr_log.txt bugs/expr_log_bug_ocaml$currentProgram.txt
            cp illtyped.ml bugs/illtyped_bug_ocaml$currentProgram.ml
        fi
    fi
    currentProgram=$((currentProgram + 1))
done