#!/usr/bin/env python3
"""
Script to create a lint report of org files, specifically checking for guile source blocks.
"""
import os
import re
import sys
import json
from datetime import datetime

def scan_file(file_path):
    """Scan a file for guile source blocks and other potential issues."""
    with open(file_path, 'r') as file:
        content = file.read()
    
    results = {
        'file': file_path,
        'guile_blocks': content.count('#+begin_src guile'),
        'scheme_blocks': content.count('#+begin_src scheme'),
        'total_code_blocks': len(re.findall(r'#\+begin_src', content)),
        'missing_end_src': len(re.findall(r'#\+begin_src', content)) - len(re.findall(r'#\+end_src', content)),
    }
    
    return results

def main():
    """Main function to process files and create a report."""
    # Find all org files
    org_files = []
    for root, dirs, files in os.walk('examples/'):
        for file in files:
            if file.endswith('.org'):
                org_files.append(os.path.join(root, file))
    
    # Process each file
    results = []
    for file_path in sorted(org_files):
        results.append(scan_file(file_path))
    
    # Create summary
    summary = {
        'timestamp': datetime.now().isoformat(),
        'total_files': len(results),
        'total_guile_blocks': sum(r['guile_blocks'] for r in results),
        'total_scheme_blocks': sum(r['scheme_blocks'] for r in results),
        'total_code_blocks': sum(r['total_code_blocks'] for r in results),
        'files_with_guile': sum(1 for r in results if r['guile_blocks'] > 0),
        'files_with_issues': sum(1 for r in results if r['missing_end_src'] > 0),
    }
    
    # Generate report
    report = {
        'summary': summary,
        'files': results
    }
    
    # Output JSON report
    with open('org_lint_report_before.json', 'w') as f:
        json.dump(report, f, indent=2)
    
    # Print text summary
    print("Org File Lint Report")
    print("===================")
    print(f"Timestamp: {summary['timestamp']}")
    print(f"Total files scanned: {summary['total_files']}")
    print(f"Total code blocks: {summary['total_code_blocks']}")
    print(f"Total guile blocks: {summary['total_guile_blocks']}")
    print(f"Total scheme blocks: {summary['total_scheme_blocks']}")
    print(f"Files with guile blocks: {summary['files_with_guile']}")
    print(f"Files with missing end_src: {summary['files_with_issues']}")
    print("\nFiles with guile blocks:")
    for r in results:
        if r['guile_blocks'] > 0:
            print(f"  {r['file']}: {r['guile_blocks']} guile blocks")
    
    print("\nDetailed report saved to org_lint_report_before.json")

if __name__ == "__main__":
    main()