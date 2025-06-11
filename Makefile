# Makefile for org-anki-blocks

EMACS ?= emacs
EMACS_BATCH = $(EMACS) --batch
LOAD_PATH = -L .
ELPA_DIR = ~/.emacs.d/elpa

.PHONY: test install-deps clean

# Run tests
test: install-deps
	@echo "Running tests..."
	$(EMACS_BATCH) \
		--eval "(setq package-user-dir \"$(ELPA_DIR)\")" \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		$(LOAD_PATH) \
		-l test/test-org-anki-blocks.el \
		-f buttercup-run-discover

# Install dependencies
install-deps:
	@echo "Installing dependencies..."
	@mkdir -p $(ELPA_DIR)
	$(EMACS_BATCH) \
		--eval "(require 'package)" \
		--eval "(setq package-user-dir \"$(ELPA_DIR)\")" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(unless (package-installed-p 'buttercup) (package-install 'buttercup))"

# Clean compiled files
clean:
	rm -f *.elc test/*.elc

# Run a single test file
test-file:
	$(EMACS_BATCH) \
		--eval "(setq package-user-dir \"$(ELPA_DIR)\")" \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		$(LOAD_PATH) \
		-l $(FILE) \
		-f buttercup-run-discover