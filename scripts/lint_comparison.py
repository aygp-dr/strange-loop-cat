#!/usr/bin/env python3
"""
Compare the before and after lint reports to show the changes.
"""
import json
import sys
from datetime import datetime

def load_report(filename):
    with open(filename, 'r') as f:
        return json.load(f)

def main():
    before = load_report("org_lint_report_before.json")
    after = load_report("org_lint_report_after.json")
    
    print("Org Source Block Conversion Summary")
    print("==================================")
    print(f"Timestamp: {datetime.now().isoformat()}")
    print("\nBefore conversion:")
    print(f"- Total guile blocks: {before['summary']['total_guile_blocks']}")
    print(f"- Total scheme blocks: {before['summary']['total_scheme_blocks']}")
    print(f"- Files with guile blocks: {before['summary']['files_with_guile']}")
    
    print("\nAfter conversion:")
    print(f"- Total guile blocks: {after['summary']['total_guile_blocks']}")
    print(f"- Total scheme blocks: {after['summary']['total_scheme_blocks']}")
    print(f"- Files with guile blocks: {after['summary']['files_with_guile']}")
    
    print("\nChanges by file:")
    for before_file in before['files']:
        file_name = before_file['file']
        after_file = next((f for f in after['files'] if f['file'] == file_name), None)
        
        if after_file:
            guile_diff = before_file['guile_blocks'] - after_file['guile_blocks']
            scheme_diff = after_file['scheme_blocks'] - before_file['scheme_blocks']
            
            if guile_diff > 0:
                print(f"- {file_name}: Converted {guile_diff} guile blocks to scheme")

if __name__ == "__main__":
    main()