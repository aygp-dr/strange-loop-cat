#!/usr/bin/env python3
"""
Markdown to Org Mode Converter for Strange Loop Cat project

This script converts Markdown guides to Org Mode format, handling:
- Adding appropriate TITLE, AUTHOR, DATE headers
- Converting Markdown headings to Org Mode syntax
- Transforming code blocks to use proper #+begin_src blocks
- Adding org-babel tangle directives for code examples
"""

import os
import re
import sys
import datetime
from pathlib import Path

def extract_title(md_content):
    """Extract title from the first heading in Markdown content"""
    title_match = re.search(r'^#\s+(.+)$', md_content, re.MULTILINE)
    if title_match:
        return title_match.group(1).strip()
    return "Untitled Guide"

def md_to_org(md_content, filename):
    """Convert Markdown content to Org Mode format"""
    # Extract title from the first heading
    title = extract_title(md_content)
    
    # Create Org headers
    current_date = datetime.datetime.now().strftime("%Y-%m-%d")
    org_headers = f"""#+TITLE: {title}
#+AUTHOR: Strange Loop Cat Project
#+DATE: {current_date}
#+PROPERTY: header-args:scheme :noweb yes :results output :exports both
#+PROPERTY: header-args:mermaid :noweb yes :file ./images/diagrams/{os.path.splitext(os.path.basename(filename))[0]}.png
#+STARTUP: showall

"""

    # Convert headings (# -> *, ## -> **, etc.)
    content = re.sub(r'^#{1}\s+(.+)$', r'* \1', md_content, flags=re.MULTILINE)
    content = re.sub(r'^#{2}\s+(.+)$', r'** \1', content, flags=re.MULTILINE)
    content = re.sub(r'^#{3}\s+(.+)$', r'*** \1', content, flags=re.MULTILINE)
    content = re.sub(r'^#{4}\s+(.+)$', r'**** \1', content, flags=re.MULTILINE)
    content = re.sub(r'^#{5}\s+(.+)$', r'***** \1', content, flags=re.MULTILINE)
    content = re.sub(r'^#{6}\s+(.+)$', r'****** \1', content, flags=re.MULTILINE)
    
    # Convert inline code (`code` -> =code=)
    content = re.sub(r'`([^`\n]+)`', r'=\1=', content)
    
    # Convert links ([text](url) -> [[url][text]])
    content = re.sub(r'\[([^\]]+)\]\(([^)]+)\)', r'[[\2][\1]]', content)
    
    # Convert bold (**text** -> *text*)
    content = re.sub(r'\*\*([^*\n]+)\*\*', r'*\1*', content)
    
    # Convert italic (*text* -> /text/)
    content = re.sub(r'(?<!\*)\*([^*\n]+)\*(?!\*)', r'/\1/', content)
    
    # Convert code blocks
    # Step 1: Find all code blocks
    code_block_pattern = re.compile(r'```(\w*)\n(.*?)```', re.DOTALL)
    
    # Function to replace code blocks with org-mode format
    def replace_code_block(match):
        lang = match.group(1).strip() or "text"
        # Map common languages to Org Mode compatible ones
        lang_map = {
            "js": "javascript",
            "ts": "typescript",
            "py": "python",
            "sh": "shell",
            "bash": "shell",
            "lisp": "scheme",
            "guile": "scheme",
        }
        lang = lang_map.get(lang.lower(), lang)
        
        code = match.group(2)
        
        # For Scheme blocks, add tangle directive
        tangle_directive = ""
        if lang.lower() == "scheme":
            base_filename = os.path.splitext(os.path.basename(filename))[0]
            tangle_path = f"../src/generated/{base_filename.replace('-guide', '')}.scm"
            tangle_directive = f":tangle {tangle_path} :mkdirp yes :noweb yes :results output :exports both "
        elif lang.lower() == "mermaid":
            base_filename = os.path.splitext(os.path.basename(filename))[0]
            tangle_directive = f":noweb yes :file ./images/diagrams/{base_filename}.png "
        
        return f"#+begin_src {lang} {tangle_directive}\n{code}#+end_src"
    
    # Replace all code blocks
    content = code_block_pattern.sub(replace_code_block, content)
    
    # Convert task lists
    content = re.sub(r'^- \[ \] (.+)$', r'- TODO \1', content, flags=re.MULTILINE)
    content = re.sub(r'^- \[x\] (.+)$', r'- DONE \1', content, flags=re.MULTILINE)
    
    # Remove the first heading since it's already in the TITLE
    content = re.sub(r'^\* .+?\n', '', content, count=1)
    
    # Combine headers and content
    return org_headers + content

def process_file(md_path, output_dir=None):
    """Process a single Markdown file and convert it to Org Mode"""
    md_path = Path(md_path)
    
    # Skip CLAUDE.md and files in issues directory
    if md_path.name == "CLAUDE.md" or "issues" in str(md_path):
        print(f"Skipping {md_path}")
        return
    
    # Determine output path
    if output_dir:
        # Create the output directory if it doesn't exist
        Path(output_dir).mkdir(parents=True, exist_ok=True)
        
        # Use the same filename but with .org extension in the output directory
        out_path = Path(output_dir) / f"{md_path.stem}.org"
    else:
        # Use the same directory as the input file
        out_path = md_path.with_suffix('.org')
    
    print(f"Converting {md_path} to {out_path}")
    
    try:
        with open(md_path, 'r', encoding='utf-8') as f:
            md_content = f.read()
        
        org_content = md_to_org(md_content, str(md_path))
        
        with open(out_path, 'w', encoding='utf-8') as f:
            f.write(org_content)
        
        print(f"✓ Converted {md_path} -> {out_path}")
        return True
    except Exception as e:
        print(f"Error converting {md_path}: {e}")
        return False

def main():
    """Main function to process command line arguments"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Convert Markdown files to Org Mode format")
    parser.add_argument("files", nargs="+", help="Markdown files or directories to convert")
    parser.add_argument("--output-dir", "-o", help="Output directory for converted files")
    parser.add_argument("--recursive", "-r", action="store_true", help="Process directories recursively")
    
    args = parser.parse_args()
    
    files_processed = 0
    success_count = 0
    
    for path in args.files:
        path = Path(path)
        
        if path.is_file() and path.suffix.lower() == '.md':
            # Process a single file
            success = process_file(path, args.output_dir)
            files_processed += 1
            if success:
                success_count += 1
        
        elif path.is_dir():
            # Process a directory
            if args.recursive:
                for md_file in path.glob('**/*.md'):
                    success = process_file(md_file, args.output_dir)
                    files_processed += 1
                    if success:
                        success_count += 1
            else:
                for md_file in path.glob('*.md'):
                    success = process_file(md_file, args.output_dir)
                    files_processed += 1
                    if success:
                        success_count += 1
    
    print(f"\nSummary: Converted {success_count}/{files_processed} Markdown files to Org Mode format")

if __name__ == "__main__":
    main()