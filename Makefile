.PHONY: all init clean mermaid docs test scheme-test tangle tangle-file md2org

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
MERMAID_FILES = $(wildcard images/diagrams/*.mmd)
MERMAID_PNGS = $(patsubst %.mmd,%.png,$(MERMAID_FILES))

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
	$(EMACS) --batch --eval "(require 'org)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--eval "(setq org-babel-tangle-create-missing-dirs-and-files t)" \
		-l tangle-babel.el

# Tangle a specific org file
tangle-file:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make tangle-file FILE=examples/filename.org"; \
	else \
		$(EMACS) --batch --eval "(require 'org)" \
			--eval "(setq org-confirm-babel-evaluate nil)" \
			--eval "(setq org-babel-tangle-create-missing-dirs-and-files t)" \
			--eval '(org-babel-tangle-file "$(FILE)")'; \
	fi

# Convert Markdown guides to Org Mode
md2org:
	@echo "Converting Markdown guides to Org Mode..."
	@python3 scripts/md2org.py -r docs/guides -o examples
	@echo "Conversion completed. Files saved to examples/ directory."
