#!/usr/bin/env python3
"""
Category analysis script for the Strange Loop Cat project.
This script demonstrates how to use the Python utilities to analyze
category theory structures from org files.
"""

import os
import sys
import json
from pathlib import Path
from typing import Dict, List, Any

from src.python.files_processor import extract_category_data, create_graphviz_diagram


def analyze_org_files(directory: str) -> Dict[str, Any]:
    """
    Analyze all org files in a directory for category theory structures.
    
    Args:
        directory: Path to directory containing org files
        
    Returns:
        Dictionary with analysis results
    """
    dir_path = Path(directory)
    if not dir_path.exists() or not dir_path.is_dir():
        return {"error": f"Directory not found: {directory}"}
    
    results = {}
    
    for org_file in dir_path.glob("*.org"):
        print(f"Analyzing {org_file.name}...")
        data = extract_category_data(str(org_file))
        
        # Count entities
        num_categories = len(data.get("categories", []))
        num_objects = len(data.get("objects", []))
        num_morphisms = len(data.get("morphisms", []))
        num_compositions = len(data.get("compositions", []))
        
        # Generate dot representation
        dot = create_graphviz_diagram(data)
        
        results[org_file.name] = {
            "categories": num_categories,
            "objects": num_objects,
            "morphisms": num_morphisms,
            "compositions": num_compositions,
            "graphviz": dot
        }
    
    return results


def main():
    """
    Main function to analyze category theory structures in org files.
    """
    # Get the repository root directory
    repo_root = Path(__file__).parent.parent.parent
    examples_dir = repo_root / "examples"
    
    if not examples_dir.exists():
        print(f"Examples directory not found: {examples_dir}")
        return 1
    
    print(f"Analyzing org files in {examples_dir}...")
    results = analyze_org_files(str(examples_dir))
    
    # Print summary
    print("\nSummary of Category Theory Structures:")
    print("=====================================")
    
    for file_name, data in results.items():
        print(f"\n{file_name}:")
        print(f"  Categories: {data['categories']}")
        print(f"  Objects: {data['objects']}")
        print(f"  Morphisms: {data['morphisms']}")
        print(f"  Compositions: {data['compositions']}")
    
    # Save results to output directory
    output_dir = repo_root / "build"
    output_dir.mkdir(exist_ok=True)
    
    output_file = output_dir / "category_analysis.json"
    with open(output_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"\nAnalysis results saved to {output_file}")
    
    # Save GraphViz dot files
    for file_name, data in results.items():
        base_name = Path(file_name).stem
        dot_file = output_dir / f"{base_name}_category.dot"
        with open(dot_file, 'w') as f:
            f.write(data['graphviz'])
        print(f"GraphViz representation saved to {dot_file}")
    
    return 0


if __name__ == "__main__":
    sys.exit(main())