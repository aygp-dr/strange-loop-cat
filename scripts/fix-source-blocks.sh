#!/bin/bash
# Script to change guile source blocks to scheme source blocks

echo "Converting guile source blocks to scheme in org files..."

# Find all org files
find examples/ -name "*.org" | while read org_file; do
  echo "Processing $org_file"
  # Replace #+begin_src guile with #+begin_src scheme
  sed -i 's/#+begin_src guile/#+begin_src scheme/g' "$org_file"
  # Count changes
  changes=$(grep -c "#+begin_src scheme" "$org_file")
  echo "  Made $changes conversions"
done

echo "Conversion complete."