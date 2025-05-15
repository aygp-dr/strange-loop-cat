#!/usr/bin/env python3
"""
Script to convert guile source blocks to scheme source blocks in org files.
"""
import os
import re
import sys

def convert_file(file_path):
    """Convert guile source blocks to scheme in the given file."""
    with open(file_path, 'r') as file:
        content = file.read()
    
    # Count original occurrences
    original_count = content.count('#+begin_src guile')
    
    # Replace occurrences
    new_content = content.replace('#+begin_src guile', '#+begin_src scheme')
    
    # Write back if changes were made
    if original_count > 0:
        with open(file_path, 'w') as file:
            file.write(new_content)
        print(f"Converted {original_count} blocks in {file_path}")
    else:
        print(f"No guile blocks found in {file_path}")
    
    return original_count

def main():
    """Main function to process files."""
    # Files to process
    files = [
        "examples/fixed-points-and-convergence.org",
        "examples/modular-systems-and-symmetry.org",
        "examples/geb-formal-systems.org"
    ]
    
    total_converted = 0
    for file_path in files:
        if os.path.exists(file_path):
            total_converted += convert_file(file_path)
        else:
            print(f"File not found: {file_path}")
    
    print(f"Total conversions: {total_converted}")

if __name__ == "__main__":
    main()