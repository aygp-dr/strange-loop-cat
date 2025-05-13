mkdir -p images/diagrams
mkdir -p src/guile
mkdir -p src/python
mkdir -p build

.PHONY: all clean mermaid docs test venv python-install python-test

# Default target
all: mermaid docs

# Variables
GUILE = guile
EMACS = emacs
MERMAID_CLI = npx @mermaid-js/mermaid-cli
MERMAID_FILES = $(wildcard images/diagrams/*.mmd)
MERMAID_PNGS = $(patsubst %.mmd,%.png,$(MERMAID_FILES))
PYTHON = python3
UV = uv

# Generate Mermaid diagrams
mermaid: $(MERMAID_PNGS)

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
	rm -rf __pycache__/
	rm -rf src/python/__pycache__/

# Run tests
test: scheme-test python-test

# Run Scheme tests
scheme-test:
	$(GUILE) -L src/guile -c "(use-modules (test-runner)) (run-all-tests)"

# Create Python virtual environment
venv:
	$(UV) venv

# Install Python dependencies
python-install:
	$(UV) pip install -r requirements.txt

# Run Python tests
python-test:
	PYTHONPATH=$(PWD) $(PYTHON) -m src.python.files_processor
	PYTHONPATH=$(PWD) $(PYTHON) -m src.python.llm_utils
