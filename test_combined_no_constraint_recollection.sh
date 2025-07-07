#!/usr/bin/bash

# for statistical purposes

for i in {1..100000}; do
    timeout 2 ./generator -no-constraint-recollection
done