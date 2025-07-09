#!/usr/bin/bash

# for statistical purposes: collecting data on evil rule locations

for i in {1..100000}; do
    timeout 2 ./generator
done