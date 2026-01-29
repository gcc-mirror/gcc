#!/bin/bash
# This script shows the corrected version that passes shellcheck

echo "Testing shellcheck with improved code"

# Fix 1: Quoted variable and proper parameter check
if [ "${1:-}" = "test" ]; then
    echo "Test mode activated"
fi

# Fix 2: Use glob instead of ls
for file in *.txt; do
    # Check if glob matched any files
    [ -e "$file" ] || continue
    echo "Processing: $file"
done

# Fix 3: Quoted variable in echo
echo "Processing file: $file"

# Fix 4: Quoted variable in comparison
if [ "$USER" = "root" ]; then
    echo "Running as root"
fi

# Fix 5: Proper assignment (this was actually fine, but adding quotes for consistency)
name="${1:-unknown}"

echo "Script completed for user: $name"
