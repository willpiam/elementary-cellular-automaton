#!/bin/bash

# This progam takes 2 files as command line arguments and print "same" when they hash to the same value and "different" when they do not

# Check if exactly two files were provided
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 file1 file2"
    exit 1
fi

# Hash both files using sha256sum and extract just the hash value (first field)
hash1=$(sha256sum "$1" | awk '{print $1}')
hash2=$(sha256sum "$2" | awk '{print $1}')

# Compare the hash values and print the result
if [ "$hash1" = "$hash2" ]; then
    echo "same"
else
    echo "different"
fi
