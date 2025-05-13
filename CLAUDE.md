# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build/Test Commands
- Generate Mermaid diagrams: `make mermaid`
- Generate documentation: `make docs`
- Run all tests: `make test`
- Run a single test: `guile -L src/guile -c "(use-modules (test-runner)) (run-test \"test-name\")"`
- Clean generated files: `make clean`

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