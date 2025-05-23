.PHONY: all init clean mermaid docs test scheme-test tangle tangle-file detangle detangle-file org-lint org-lint-file md2org md2org-file

# Default target
all: mermaid docs

# Project initialization
init: images/diagrams build src/generated
	npm install

# Create output directories as needed
build:
	mkdir -p $@

images/diagrams:
	mkdir -p $@
	
src/generated:
	mkdir -p $@

# Variables
GUILE = guile3
EMACS = emacs
MERMAID_CLI = npx @mermaid-js/mermaid-cli
MERMAID_FILES = $(wildcard images/diagrams/*.mmd) $(wildcard docs/diagrams/*.mmd)
MERMAID_PNGS = $(patsubst %.mmd,%.png,$(MERMAID_FILES))
GUIDE_FILES = $(wildcard examples/*-guide.org)

# Generate Mermaid diagrams
mermaid: images/diagrams $(MERMAID_PNGS)

%.png: %.mmd
	$(MERMAID_CLI) -i $< -o $@

# Generate documentation
docs:
	$(EMACS) --batch --eval "(require 'org)" --eval "(setq org-confirm-babel-evaluate nil)" \
		--eval "(org-babel-tangle-file \"index.org\")" \
		--eval "(org-babel-tangle-file \"categories.org\")" \
		--eval "(org-babel-tangle-file \"functors.org\")" \
		--eval "(org-babel-tangle-file \"naturality.org\")"
	$(EMACS) --batch --eval "(require 'org)" --eval "(setq org-confirm-babel-evaluate nil)" \
		--eval "(org-html-export-to-html-batch \"index.org\")" \
		--eval "(org-html-export-to-html-batch \"categories.org\")" \
		--eval "(org-html-export-to-html-batch \"functors.org\")" \
		--eval "(org-html-export-to-html-batch \"naturality.org\")"

# Clean generated files
clean:
	rm -f *.html
	rm -f images/diagrams/*.png
	rm -rf build/*
	rm -rf src/generated/*

# Run tests
test: scheme-test

# Run Scheme tests
scheme-test:
	$(GUILE) -L src/guile -c "(use-modules (test-runner)) (run-all-tests)"

# Tangle org files into Scheme files
tangle: build
	@echo "Tangling core org files..."
	$(EMACS) --batch -l scripts/tangle-config.el -l tangle-babel.el
	@echo "Tangling guide files..."
	$(EMACS) --batch -l scripts/tangle-config.el --eval "(tangle-files '($(foreach file,$(GUIDE_FILES),\"$(file)\" )))"

# Tangle a specific org file
tangle-file:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make tangle-file FILE=examples/filename.org"; \
	else \
		$(EMACS) --batch -l scripts/tangle-config.el --eval '(tangle-file "$(FILE)")'; \
	fi

# Convert Markdown guides to Org Mode
md2org:
	@echo "Converting Markdown guides to Org Mode..."
	@python3 scripts/md2org.py -r docs/guides -o examples
	@echo "Conversion completed. Files saved to examples/ directory."

# Convert a single Markdown file to Org Mode
md2org-file:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make md2org-file FILE=path/to/file.md [OUTPUT=output/dir]"; \
	else \
		if [ -z "$(OUTPUT)" ]; then \
			python3 scripts/md2org.py "$(FILE)"; \
		else \
			python3 scripts/md2org.py "$(FILE)" -o "$(OUTPUT)"; \
		fi; \
	fi

# Detangle org files (update code blocks from tangled files)
detangle:
	@echo "Detangling org files..."
	$(EMACS) --batch -l scripts/tangle-config.el -l tangle-babel.el --eval "(detangle-files '($(foreach file,$(wildcard examples/*.org),\"$(file)\" )))"
	@echo "Detangling guide files..."
	$(EMACS) --batch -l scripts/tangle-config.el --eval "(detangle-files '($(foreach file,$(GUIDE_FILES),\"$(file)\" )))"

# Detangle a specific org file
detangle-file:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make detangle-file FILE=examples/filename.org"; \
	else \
		$(EMACS) --batch -l scripts/tangle-config.el --eval '(detangle-file "$(FILE)")'; \
	fi

# Run org-lint on all org files
org-lint:
	@echo "Running org-lint on org files..."
	$(EMACS) --batch -l scripts/tangle-config.el --eval "(lint-files '($(foreach file,$(wildcard examples/*.org),\"$(file)\" )))"
	@echo "Running org-lint on guide files..."
	$(EMACS) --batch -l scripts/tangle-config.el --eval "(lint-files '($(foreach file,$(GUIDE_FILES),\"$(file)\" )))"

# Run org-lint on a specific org file
org-lint-file:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make org-lint-file FILE=examples/filename.org"; \
	else \
		$(EMACS) --batch -l scripts/tangle-config.el --eval '(lint-file "$(FILE)")'; \
	fi
