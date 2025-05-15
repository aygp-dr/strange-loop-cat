#!/usr/bin/env python3
"""
Script to convert guile source blocks to scheme source blocks in org files.
Uses regular expressions for precise replacements rather than orgparse,
as orgparse doesn't preserve the exact formatting we need.
"""
import os
import re
import sys
from datetime import datetime

def convert_file(file_path):
    """
    Convert guile source blocks to scheme in the given file.
    """
    with open(file_path, 'r') as file:
        content = file.read()
    
    # Use regex to ensure we're only replacing the exact source block headers
    pattern = r'(#\+begin_src\s+)guile(\s*|$)'
    replacement = r'\1scheme\2'
    
    # Count original occurrences before replacing
    matches = re.findall(pattern, content)
    count = len(matches)
    
    # Replace occurrences
    new_content = re.sub(pattern, replacement, content)
    
    # Check if content changed
    if content != new_content:
        # Create backup
        backup_path = f"{file_path}.bak"
        with open(backup_path, 'w') as backup:
            backup.write(content)
        
        # Write updated content
        with open(file_path, 'w') as file:
            file.write(new_content)
        
        print(f"✓ Converted {count} blocks in {file_path} (backup saved)")
        return count
    else:
        print(f"- No changes made to {file_path}")
        return 0

def main():
    """Main function to process files."""
    # Files with guile blocks according to our linter
    files_to_process = [
        "examples/fixed-points-and-convergence.org",
        "examples/geb-formal-systems.org", 
        "examples/modular-systems-and-symmetry.org"
    ]
    
    # Log file for changes
    log_file = f"conversion_log_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
    with open(log_file, 'w') as log:
        log.write(f"Conversion log - {datetime.now().isoformat()}\n")
        log.write("="*60 + "\n\n")
        
        total = 0
        for file_path in files_to_process:
            if os.path.exists(file_path):
                count = convert_file(file_path)
                total += count
                log.write(f"{file_path}: {count} conversions\n")
            else:
                log.write(f"{file_path}: FILE NOT FOUND\n")
                print(f"! File not found: {file_path}")
        
        log.write(f"\nTotal conversions: {total}\n")
        print(f"\nTotal conversions: {total}")
        print(f"Log saved to: {log_file}")

if __name__ == "__main__":
    main()