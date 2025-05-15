#!/usr/bin/env python3
"""
Script to fix Guile 3 compatibility issues in tangled Scheme files.
"""
import os
import re
import sys
from datetime import datetime

def fix_file(file_path):
    """
    Fix Guile 3 compatibility issues in a file.
    """
    with open(file_path, 'r') as file:
        content = file.read()
    
    # Keep track of total fixes
    total_fixes = 0
    
    # Fix keyword argument syntax (#:optional #:key)
    pattern1 = r'(define \([\w-]+ [^)]+) #:optional #:key'
    replacement1 = r'\1 #:optional'
    
    # Count original occurrences
    count1 = len(re.findall(pattern1, content))
    total_fixes += count1
    
    # Replace occurrences
    content = re.sub(pattern1, replacement1, content)
    
    # Fix optional arguments to use define*
    pattern2 = r'(define) (\([\w-]+ [^)]+) #:optional'
    replacement2 = r'\1* \2 #:optional'
    
    # Count original occurrences
    count2 = len(re.findall(pattern2, content))
    total_fixes += count2
    
    # Replace occurrences
    content = re.sub(pattern2, replacement2, content)
    
    # Add srfi-11 module for let-values if needed
    if 'let-values' in content and '(srfi srfi-11)' not in content:
        pattern3 = r'(use-modules\s+\([^)]+\))'
        replacement3 = r'\1\n             (srfi srfi-11)    ; let-values'
        
        # Count original occurrences
        count3 = 1 if re.search(pattern3, content) else 0
        total_fixes += count3
        
        # Replace occurrences
        content = re.sub(pattern3, replacement3, content)
    
    if total_fixes == 0:
        print(f"No compatibility issues found in {file_path}")
        return 0
    
    # Create backup
    backup_path = f"{file_path}.compat.bak"
    with open(backup_path, 'w') as backup:
        backup.write(content)
    
    # Write updated content
    with open(file_path, 'w') as file:
        file.write(content)
    
    print(f"✓ Fixed {total_fixes} compatibility issues in {file_path} (backup saved)")
    return total_fixes

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
        total_fixed += fix_file(file_path)
    
    print(f"\nTotal compatibility issues fixed: {total_fixed}")

if __name__ == "__main__":
    main()