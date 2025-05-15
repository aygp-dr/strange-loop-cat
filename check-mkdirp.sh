#!/bin/bash
# Script to check for tangle directives without :mkdirp yes

echo "Checking for tangle directives without :mkdirp yes..."
echo ""

# Find all org files
find examples/ -name "*.org" | while read org_file; do
  echo "Checking $org_file:"
  missing_mkdirp=$(grep -n "#+begin_src" "$org_file" | grep "tangle" | grep -v "mkdirp" || true)
  
  if [ -n "$missing_mkdirp" ]; then
    echo "  WARN: Missing :mkdirp yes in these lines:"
    echo "$missing_mkdirp" | sed 's/^/    /'
  else
    echo "  OK: All tangle directives have :mkdirp yes"
  fi
  echo ""
done

echo "Check complete."