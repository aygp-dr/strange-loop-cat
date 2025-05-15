#!/usr/bin/env python3
"""
Script to remove guile header args from org files.
"""
import os
import re
import sys
from datetime import datetime

def process_file(file_path):
    """
    Remove guile header args from the given file.
    """
    with open(file_path, 'r') as file:
        content = file.read()
    
    # Count original occurrences
    original_headers = content.count('#+PROPERTY: header-args:guile')
    
    # Skip if no guile headers found
    if original_headers == 0:
        print(f"No guile headers found in {file_path}")
        return 0
    
    # Create backup
    backup_path = f"{file_path}.headers.bak"
    with open(backup_path, 'w') as backup:
        backup.write(content)
    
    # Remove the guile header args line
    new_content = re.sub(r'^\#\+PROPERTY: header-args:guile.*$', '', content, flags=re.MULTILINE)
    
    # Write updated content
    with open(file_path, 'w') as file:
        file.write(new_content)
    
    print(f"✓ Removed {original_headers} guile header args from {file_path} (backup saved)")
    return original_headers

def main():
    """Main function to process all org files."""
    org_files = []
    for root, dirs, files in os.walk('examples/'):
        for file in files:
            if file.endswith('.org'):
                org_files.append(os.path.join(root, file))
    
    # Process each file
    total_removed = 0
    for file_path in sorted(org_files):
        total_removed += process_file(file_path)
    
    print(f"\nTotal guile headers removed: {total_removed}")

if __name__ == "__main__":
    main()