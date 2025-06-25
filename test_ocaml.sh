#!/usr/bin/bash

currentProgram=0
ocamlopt -O2 helpers.ml types.ml tracemem.ml prettyprinter.ml generation.ml
while true; do
    timeout 2 ./a.out -ocaml
    ocamlc -o illtyped illtyped.ml
    if [ $? -eq 0 ]; then # status code 0 denotes successful compilation
        cp expr_log.txt expr_log_bug$currentProgram.txt
        cp illtyped.ml illtyped_bug$currentProgram.ml
        currentProgram=$((currentProgram + 1))
    fi
done