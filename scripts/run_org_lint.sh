#!/bin/bash
# Run org-lint on converted org files to check for formatting issues

# Directory with converted org files
ORG_DIR="/home/jwalsh/projects/aygp-dr/strange-loop-cat/examples"

# Path to emacs
EMACS="emacs"

# Generate the report
echo "Running org-lint on converted files..."
echo ""

for org_file in "$ORG_DIR"/*-guide.org; do
    if [ -f "$org_file" ]; then
        echo "Checking $(basename "$org_file")..."
        $EMACS --batch --eval "(progn
                (require 'org)
                (require 'org-lint)
                (find-file \"$org_file\")
                (let ((issues (org-lint)))
                  (if issues
                      (progn
                        (message \"Issues found:\")
                        (dolist (issue issues)
                          (message \"%s\" issue)))
                    (message \"No issues found.\"))))" 2>&1
        echo ""
    fi
done

echo "Org-lint check completed."