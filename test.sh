#!/usr/bin/env bash

# Test runner for org-anki-blocks

echo "Running org-anki-blocks test suite..."
echo "=================================="

# Create a temporary directory for ELPA packages if needed
ELPA_DIR="${HOME}/.emacs.d/elpa"
mkdir -p "$ELPA_DIR"

# First, try to install buttercup if not available
echo "Checking for buttercup..."
emacs --batch \
      --eval "(require 'package)" \
      --eval "(setq package-user-dir \"$ELPA_DIR\")" \
      --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
      --eval "(package-initialize)" \
      --eval "(unless (package-installed-p 'buttercup) (package-refresh-contents) (package-install 'buttercup))" \
      2>/dev/null || {
    echo "Note: Could not auto-install buttercup. Attempting to run tests anyway..."
}

# Run the tests with proper load paths
echo "Running tests..."
emacs --batch \
      --eval "(setq package-user-dir \"$ELPA_DIR\")" \
      --eval "(require 'package)" \
      --eval "(package-initialize)" \
      -L . \
      -l test/test-org-anki-blocks.el \
      -f buttercup-run-discover

# Capture exit code
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    echo ""
    echo "✅ All tests passed!"
else
    echo ""
    echo "❌ Some tests failed!"
fi

exit $EXIT_CODE