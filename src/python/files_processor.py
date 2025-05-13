#!/usr/bin/env python3
"""
Files processor for the Strange Loop Cat project.
This module provides utilities for processing org files and extracting
information about categories, objects, and morphisms.
"""

import re
import os
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Set, Any


def extract_category_data(org_file_path: str) -> Dict[str, Any]:
    """
    Extract category theory data from an org file.
    
    Args:
        org_file_path: Path to the org file
        
    Returns:
        Dictionary containing extracted category data
    """
    if not os.path.exists(org_file_path):
        return {"error": f"File not found: {org_file_path}"}
        
    with open(org_file_path, 'r') as f:
        content = f.read()
    
    # Extract categories
    categories = []
    cat_pattern = r'define\s+cat-(\w+)\s+\(make-category\s+"([^"]+)"\s+'
    cat_matches = re.finditer(cat_pattern, content)
    
    for match in cat_matches:
        var_name = match.group(1)
        cat_name = match.group(2)
        categories.append({"var_name": var_name, "name": cat_name})
    
    # Extract objects
    objects = []
    obj_pattern = r'define\s+(\w+)\s+\(make-object\s+"([^"]+)"\)'
    obj_matches = re.finditer(obj_pattern, content)
    
    for match in obj_matches:
        var_name = match.group(1)
        obj_name = match.group(2)
        objects.append({"var_name": var_name, "name": obj_name})
    
    # Extract morphisms
    morphisms = []
    morph_pattern = r'define\s+(\w+)\s+\(make-morphism\s+"([^"]+)"\s+(\w+)\s+(\w+)\)'
    morph_matches = re.finditer(morph_pattern, content)
    
    for match in morph_matches:
        var_name = match.group(1)
        morph_name = match.group(2)
        domain_var = match.group(3)
        codomain_var = match.group(4)
        morphisms.append({
            "var_name": var_name,
            "name": morph_name,
            "domain_var": domain_var,
            "codomain_var": codomain_var
        })
    
    # Extract compositions
    compositions = []
    comp_pattern = r'define\s+(\w+)\s+\(compose\s+(\w+)\s+(\w+)\)'
    comp_matches = re.finditer(comp_pattern, content)
    
    for match in comp_matches:
        var_name = match.group(1)
        first_morph = match.group(2)
        second_morph = match.group(3)
        compositions.append({
            "var_name": var_name,
            "first_morphism": first_morph,
            "second_morphism": second_morph
        })
    
    return {
        "categories": categories,
        "objects": objects,
        "morphisms": morphisms,
        "compositions": compositions
    }


def create_graphviz_diagram(category_data: Dict[str, Any]) -> str:
    """
    Create a Graphviz DOT representation of the category data.
    
    Args:
        category_data: Dictionary containing category data
        
    Returns:
        Graphviz DOT representation as a string
    """
    dot = ["digraph Category {"]
    dot.append("  rankdir=LR;")
    dot.append("  node [shape=circle, style=filled, fillcolor=lightblue];")
    
    # Add nodes for objects
    for obj in category_data.get("objects", []):
        dot.append(f'  "{obj["name"]}" [label="{obj["name"]}"];')
    
    # Add edges for morphisms
    for morph in category_data.get("morphisms", []):
        # Find the actual object names
        domain_name = next((obj["name"] for obj in category_data["objects"] 
                            if obj["var_name"] == morph["domain_var"]), morph["domain_var"])
        codomain_name = next((obj["name"] for obj in category_data["objects"] 
                              if obj["var_name"] == morph["codomain_var"]), morph["codomain_var"])
        
        dot.append(f'  "{domain_name}" -> "{codomain_name}" [label="{morph["name"]}"];')
    
    dot.append("}")
    return "\n".join(dot)


if __name__ == "__main__":
    # Example usage
    examples_dir = Path(__file__).parent.parent.parent / "examples"
    example_file = examples_dir / "basics.org"
    
    if example_file.exists():
        data = extract_category_data(str(example_file))
        print(f"Found {len(data['objects'])} objects and {len(data['morphisms'])} morphisms")
        
        # Generate GraphViz dot file
        dot_content = create_graphviz_diagram(data)
        print("\nGraphViz DOT representation:")
        print(dot_content)
    else:
        print(f"Example file not found: {example_file}")