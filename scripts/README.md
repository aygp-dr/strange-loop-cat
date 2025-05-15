# Project Scripts

This directory contains utility scripts for maintaining and managing the Strange Loop Cat project.

## Markdown to Org Mode Conversion

### md2org.py

Converts Markdown guides to Org Mode format with proper header args and tangle directives.

Usage:
```
python3 md2org.py [-r] [-o OUTPUT_DIR] FILE_OR_DIR [FILE_OR_DIR...]
```

Options:
- `-r`, `--recursive`: Process directories recursively
- `-o`, `--output-dir`: Specify output directory for converted files

Example:
```
# Convert a single file
python3 md2org.py docs/guides/yoneda-lemma-guide.md -o examples/

# Convert all guides in a directory
python3 md2org.py -r docs/guides -o examples/
```

Features:
- Adds proper Org Mode headers (TITLE, AUTHOR, DATE)
- Converts headings (# → *, ## → **, etc.)
- Transforms code blocks with proper begin/end tags
- Adds tangle directives for Scheme code blocks
- Maps file directives for Mermaid diagrams
- Converts inline formatting (bold, italic, code, links)

## Org Mode Linting and Verification

### verify_org_blocks.py

Verifies code blocks in Org Mode files to ensure they have proper header args.

Usage:
```
python3 verify_org_blocks.py FILE_OR_DIR [FILE_OR_DIR...]
```

Features:
- Verifies that Scheme blocks have proper `:noweb yes` and `:tangle` directives
- Ensures Mermaid blocks have proper `:file` directives
- Checks for mismatched begin/end src tags

### run_org_lint.sh

Runs org-lint on converted Org Mode files to check for formatting issues.

Usage:
```
./run_org_lint.sh
```

Features:
- Uses Emacs in batch mode to run org-lint
- Reports issues found in each file

## Source Block Conversion

### convert_blocks.py

Converts source block language tags (e.g., guile → scheme) in Org Mode files.

Usage:
```
python3 convert_blocks.py FILE_OR_DIR [FILE_OR_DIR...]
```

Features:
- Converts `#+begin_src guile` to `#+begin_src scheme`
- Creates backups before modifying files
- Reports number of blocks converted

### remove_guile_headers.py

Removes guile-specific header args from org files.

Usage:
```
python3 remove_guile_headers.py FILE_OR_DIR [FILE_OR_DIR...]
```

Features:
- Removes `:guile` and similar header args from source blocks
- Creates backups before modifying files

## Guile 3 Compatibility

### fix_guile3_compatibility.py

Fixes Guile 3 compatibility issues in Scheme files.

Usage:
```
python3 fix_guile3_compatibility.py FILE_OR_DIR [FILE_OR_DIR...]
```

Features:
- Converts `define` to `define*` for optional parameters
- Fixes `#:optional #:key` syntax issues
- Adds srfi-11 module for let-values support
- Creates backups before modifying files