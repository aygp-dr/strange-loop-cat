# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build/Test Commands
- Generate Mermaid diagrams: `make mermaid`
- Generate documentation: `make docs`
- Run all tests: `make test`
- Run a single test: `guile -L src/guile -c "(use-modules (test-runner)) (run-test \"test-name\")"`
- Clean generated files: `make clean`
- Tangle org files to generate Scheme code: `make tangle`
- Initialize project (one-time setup): `make init`
- Run all build processes: `make all`
- Run a generated Scheme file: `guile src/generated/file-name.scm`
- Tangle a specific org file: `make tangle-file FILE=examples/filename.org`
- Convert Markdown guides to Org Mode: `make md2org`
- Check Guile version: `guile --version` (Guile 3.x is used in this project)

## Code Style Guidelines
- Use Scheme/Guile conventions (kebab-case for names)
- Define modules with clear exports using `define-module` and `#:export`
- Document functions with preceding comments
- Maintain 2-space indentation
- Follow functional programming paradigms
- Organize code hierarchically by category concepts
- Keep functions pure when possible
- Handle errors by returning meaningful values rather than exceptions
- For diagrams, use Mermaid syntax with consistent node/edge styling
- Org files should include appropriate header args (`:noweb yes :eval yes`)
- Ensure all Org files have proper #+PROPERTY settings for Babel compatibility
- When adding new source blocks in org files, maintain the tangling structure with appropriate `:tangle` directives

## Project Architecture

This project explores category theory concepts through interactive Org-mode notebooks and Guile Scheme implementations. The architecture consists of:

### Core Components
1. **Interactive Notebooks** (examples/*.org)
   - Self-contained explorations of category theory concepts
   - Contain executable Scheme code blocks
   - Can be tangled to generate Scheme source files
   - Include Mermaid diagrams for visualization

2. **Scheme Implementation** (src/guile/*.scm)
   - Implements core category theory concepts
   - Provides the foundation for the interactive notebooks
   - Uses functional programming paradigms

3. **Generated Code** (src/generated/*.scm)
   - Created automatically by tangling Org files
   - Generated from code blocks in the notebooks
   - Used for testing and demonstration

4. **Mermaid Diagrams** (docs/diagrams/*.mmd)
   - Visual representations of categorical concepts
   - Generated from Mermaid code blocks in Org files
   - Compiled to PNG images with `make mermaid`

### Workflow
1. Edit Org files in examples/ directory
2. Run `make tangle` to generate Scheme code
3. Run `make mermaid` to generate diagrams
4. Run `make test` to verify implementations
5. Run `make docs` to generate HTML documentation
6. Use `make md2org` to convert Markdown guides from docs/guides/ to Org format in examples/

### Key Concepts
- Categories, objects, and morphisms (implemented in category.scm)
- Composition of morphisms and identity morphisms
- Functors between categories
- Natural transformations
- Strange loops and self-reference

### Development Notes
- The project is structured around literate programming using Org mode
- Code blocks in .org files tangle to create Scheme implementation files
- The core category theory implementation is in src/guile/category.scm
- Exercise solutions should be placed in src/generated/exercises/
- Diagrams are defined using Mermaid and should be placed in docs/diagrams/
- Markdown guides in docs/guides/ can be converted to Org Mode using `make md2org`
- The comprehensive guides cover various aspects of category theory and are converted from Markdown to Org format
- For developing new features, follow the existing pattern of:
  1. Add explanatory content in an org file
  2. Implement code in Scheme code blocks with proper tangle directives
  3. Add visualization using Mermaid diagrams
  4. Use the make system to generate all derived files

To explore a new concept, start by examining the relevant notebook in the examples/ directory, which contains both explanations and executable code.

### Guile 3 Compatibility
The project uses Guile 3.x as its Scheme implementation. Some files have compatibility changes marked with .compat.bak extensions. If working with Guile-specific features, ensure compatibility with Guile 3.x.