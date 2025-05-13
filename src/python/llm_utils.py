#!/usr/bin/env python3
"""
LLM utilities for the Strange Loop Cat project.
This module provides functions for interacting with LLMs and generating
category theory content programmatically.
"""

import os
import json
from typing import Dict, List, Optional, Union, Any
from pathlib import Path

try:
    import llm
    from files_to_prompt import convert_files_to_prompt
except ImportError:
    print("LLM packages not installed. Please run:")
    print("uv pip install llm files-to-prompt")


def generate_category_description(name: str, objects: List[str], morphisms: Dict[str, tuple]) -> str:
    """
    Generate a natural language description of a category using an LLM.
    
    Args:
        name: The name of the category
        objects: List of object names
        morphisms: Dictionary mapping morphism names to (domain, codomain) tuples
        
    Returns:
        A natural language description of the category
    """
    try:
        models = llm.get_models()
        
        # Check if models is a dictionary (normal case) or list (error case)
        if isinstance(models, dict):
            available_models = list(models.keys())
        else:
            return f"No LLM models available. Please install and configure llm package."
        
        if not available_models:
            return f"No LLM models available. Please install and configure llm package."
            
        # Build prompt
        prompt = f"""
        Describe the category '{name}' with:
        
        Objects: {', '.join(objects)}
        
        Morphisms:
        """
        
        for morph_name, (domain, codomain) in morphisms.items():
            prompt += f"- {morph_name}: {domain} → {codomain}\n"
        
        prompt += "\nDescribe this category's structure, any special properties, and relationships in formal category theory terms."
        
        try:
            # Use the first available model
            model = models[available_models[0]]
            response = model.prompt(prompt)
            return response.text()
        except Exception as e:
            return f"Error generating description: {str(e)}"
    except (AttributeError, ImportError, Exception) as e:
        return f"LLM functionality not available: {str(e)}"


def convert_org_to_prompt(file_path: str) -> str:
    """
    Convert an org file to a prompt for an LLM using files-to-prompt.
    
    Args:
        file_path: Path to the org file
        
    Returns:
        A formatted string suitable for sending to an LLM
    """
    try:
        path = Path(file_path)
        if not path.exists():
            return f"File not found: {file_path}"
            
        prompt = convert_files_to_prompt([file_path])
        return prompt
    except Exception as e:
        return f"Error converting file to prompt: {str(e)}"


if __name__ == "__main__":
    # Example usage
    category_name = "SetFinite"
    category_objects = ["Empty", "Singleton", "Boolean", "Three"]
    category_morphisms = {
        "inclusion_empty_singleton": ("Empty", "Singleton"),
        "true": ("Singleton", "Boolean"),
        "false": ("Singleton", "Boolean"),
        "identity_boolean": ("Boolean", "Boolean")
    }
    
    description = generate_category_description(
        category_name, category_objects, category_morphisms
    )
    
    print(description)