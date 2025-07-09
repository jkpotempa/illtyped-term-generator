#!/usr/bin/bash

# for statistical purposes: false positive finding when not using constraint recollection

for i in {1..100000}; do
    timeout 2 ./generator -no-constraint-recollection
done