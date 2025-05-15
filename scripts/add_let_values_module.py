#!/usr/bin/env python3
"""
Script to add srfi-11 module for let-values to all Scheme files.
"""
import os
import re
import sys
from datetime import datetime

def add_srfi_11(file_path):
    """
    Add srfi-11 module for let-values if needed.
    """
    with open(file_path, 'r') as file:
        content = file.read()
    
    # Add srfi-11 module for let-values if needed
    if 'let-values' in content and '(srfi srfi-11)' not in content:
        pattern = r'(use-modules\s+\([^)]+\))'
        replacement = r'\1\n             (srfi srfi-11)    ; let-values'
        
        # Check if there's a match
        if not re.search(pattern, content):
            print(f"No use-modules statement found in {file_path}")
            return 0
        
        # Create backup
        backup_path = f"{file_path}.srfi11.bak"
        with open(backup_path, 'w') as backup:
            backup.write(content)
        
        # Replace occurrences
        new_content = re.sub(pattern, replacement, content)
        
        # Write updated content
        with open(file_path, 'w') as file:
            file.write(new_content)
        
        print(f"✓ Added srfi-11 module to {file_path} (backup saved)")
        return 1
    else:
        if 'let-values' in content:
            print(f"let-values exists but srfi-11 already imported in {file_path}")
        else:
            print(f"let-values not used in {file_path}")
        return 0

def main():
    """Main function to process files."""
    # Find all tangled Scheme files
    scheme_files = []
    for root, dirs, files in os.walk('src/generated/'):
        for file in files:
            if file.endswith('.scm'):
                scheme_files.append(os.path.join(root, file))
    
    # Process each file
    total_fixed = 0
    for file_path in sorted(scheme_files):
        total_fixed += add_srfi_11(file_path)
    
    print(f"\nTotal files updated: {total_fixed}")

if __name__ == "__main__":
    main()