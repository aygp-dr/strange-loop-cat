#!/usr/bin/env python3
"""
Verify org-mode code blocks in converted files.

This script ensures that:
1. All code blocks have proper begin/end tags
2. Scheme blocks have appropriate tangle directives
3. Mermaid blocks have proper file directives
"""

import os
import re
import sys
from pathlib import Path

def check_code_blocks(file_path):
    """Check code blocks in an org file."""
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Get the filename without extension
    base_name = os.path.splitext(os.path.basename(file_path))[0]
    
    # Check for code blocks
    begin_blocks = re.findall(r'#\+begin_src\s+(\w+)(.*?)\n', content, re.IGNORECASE)
    end_blocks = re.findall(r'#\+end_src', content, re.IGNORECASE)
    
    issues = []
    
    # Check if begin and end blocks match
    if len(begin_blocks) != len(end_blocks):
        issues.append(f"Mismatched begin/end blocks: {len(begin_blocks)} begins, {len(end_blocks)} ends")
    
    # Check each code block
    for lang, options in begin_blocks:
        if lang.lower() == 'scheme':
            # Verify scheme blocks have tangle directives
            if ':tangle' not in options:
                issues.append(f"Scheme block without tangle directive: {options}")
            if ':noweb yes' not in options and ':noweb' not in options:
                issues.append(f"Scheme block missing ':noweb yes' option: {options}")
        
        elif lang.lower() == 'mermaid':
            # Verify mermaid blocks have file directives
            if ':file' not in options:
                issues.append(f"Mermaid block without file directive: {options}")
    
    return issues

def main():
    """Main function."""
    if len(sys.argv) < 2:
        print("Usage: python verify_org_blocks.py <org_file_or_directory> [<org_file_or_directory> ...]")
        return 1
    
    paths = sys.argv[1:]
    all_issues = {}
    
    for path_str in paths:
        path = Path(path_str)
        
        if path.is_file() and path.suffix.lower() == '.org':
            issues = check_code_blocks(path)
            if issues:
                all_issues[str(path)] = issues
        
        elif path.is_dir():
            for org_file in path.glob('**/*.org'):
                issues = check_code_blocks(org_file)
                if issues:
                    all_issues[str(org_file)] = issues
    
    # Print results
    if all_issues:
        print("\nIssues found in code blocks:")
        for file_path, issues in all_issues.items():
            print(f"\n{os.path.basename(file_path)}:")
            for issue in issues:
                print(f"  - {issue}")
        return 1
    else:
        print("\nNo issues found in code blocks.")
        return 0

if __name__ == "__main__":
    sys.exit(main())