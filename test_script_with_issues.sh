#!/bin/bash
# This script contains common issues that shellcheck will detect

echo "Testing shellcheck with problematic code"

# Issue 1: Unquoted variable
if [ $1 = "test" ]; then
    echo "Test mode activated"
fi

# Issue 2: Useless use of ls
for file in $(ls *.txt); do
    echo "Processing: $file"
done

# Issue 3: Unquoted variable in echo
echo Processing file: $file

# Issue 4: Dangerous comparison
if [ $USER = root ]; then
    echo "Running as root"
fi

# Issue 5: Missing quotes in assignment
name=$1

echo "Script completed for user: $name"
