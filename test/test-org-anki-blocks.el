;;; test-org-anki-blocks.el --- Buttercup tests for org-anki-blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Myke Bilyanskyy
;; Keywords: test, anki, org-mode

;;; Commentary:

;; Buttercup tests for org-anki-blocks package.
;; Tests all functions that don't require AnkiConnect API.

;;; Code:

(require 'buttercup)
(require 'org)

;; Add the package to load path
(let ((project-root (file-name-directory (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path project-root)
  (require 'org-anki-blocks)
  (require 'org-anki-blocks-core)
  (require 'org-anki-blocks-sync))

;;; Test Utilities

(defmacro with-org-anki-test-buffer (content &rest body)
  "Create a temporary org buffer with CONTENT and execute BODY."
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Block Finding Tests

(describe "org-anki-blocks--find-all"
  (it "finds all anki blocks in buffer"
    (with-org-anki-test-buffer
     "* Test
#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question 1
  * Back
    Answer 1
#+end_anki

Some text

#+begin_anki :deck \"Test\" :type Cloze
  * Text
    This is {{c1::cloze}} text
#+end_anki"
     (let ((blocks (org-anki-blocks--find-all)))
       (expect (length blocks) :to-be 2))))

  (it "returns empty list when no blocks found"
    (with-org-anki-test-buffer
     "* Just regular org content
No anki blocks here."
     (let ((blocks (org-anki-blocks--find-all)))
       (expect (length blocks) :to-be 0))))

  (it "ignores non-anki blocks"
    (with-org-anki-test-buffer
     "#+begin_src elisp
(message \"hello\")
#+end_src

#+begin_example
Example text
#+end_example

#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question
  * Back
    Answer
#+end_anki"
     (let ((blocks (org-anki-blocks--find-all)))
       (expect (length blocks) :to-be 1)))))

(describe "org-anki-blocks--find-at-point"
  (it "finds block when point is inside anki block"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question
  * Back
    Answer
#+end_anki"
     (goto-char (point-min))
     (forward-line 2) ; Move to "Question" line
     (let ((block (org-anki-blocks--find-at-point)))
       (expect block :not :to-be nil)
       (expect (org-element-property :type block) :to-equal "anki"))))

  (it "returns nil when point is outside anki blocks"
    (with-org-anki-test-buffer
     "* Regular heading
Some text here
#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question
#+end_anki"
     (goto-char (point-min))
     (forward-line 1) ; Move to "Some text here"
     (let ((block (org-anki-blocks--find-at-point)))
       (expect block :to-be nil)))))

(describe "org-anki-blocks--find-by-id"
  (it "finds block by string ID"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic :id 12345
  * Front
    Question
  * Back
    Answer
#+end_anki

#+begin_anki :deck \"Test\" :type Basic :id 67890
  * Front
    Question 2
  * Back
    Answer 2
#+end_anki"
     (let ((block (org-anki-blocks--find-by-id "12345")))
       (expect block :not :to-be nil)
       (expect (org-anki-blocks--get-property block :id) :to-equal "12345"))))

  (it "finds block by numeric ID"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic :id 12345
  * Front
    Question
#+end_anki"
     (let ((block (org-anki-blocks--find-by-id 12345)))
       (expect block :not :to-be nil))))

  (it "returns nil when ID not found"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic :id 12345
  * Front
    Question
#+end_anki"
     (let ((block (org-anki-blocks--find-by-id "99999")))
       (expect block :to-be nil)))))

;;; Property Management Tests

(describe "org-anki-blocks--parse-header"
  (it "parses header properties correctly"
    (with-org-anki-test-buffer
     "#+begin_anki :deck Test-Deck :type Basic :id 12345 :tags tag1,tag2
  * Front
    Question
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (props (org-anki-blocks--parse-header block)))
       (expect (plist-get props :deck) :to-equal "Test-Deck")
       (expect (plist-get props :type) :to-equal "Basic")
       (expect (plist-get props :id) :to-equal "12345")
       (expect (plist-get props :tags) :to-equal "tag1,tag2"))))

  (it "handles empty properties"
    (with-org-anki-test-buffer
     "#+begin_anki
  * Front
    Question
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (props (org-anki-blocks--parse-header block)))
       (expect props :to-be nil))))

  (it "handles properties with missing values"
    (with-org-anki-test-buffer
     "#+begin_anki :deck Test :type Basic :some-extra
  * Front
    Question
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (props (org-anki-blocks--parse-header block)))
       (expect (plist-get props :deck) :to-equal "Test")
       (expect (plist-get props :type) :to-equal "Basic")
       (expect (plist-get props :some-extra) :to-be nil)))))

(describe "org-anki-blocks--get-property"
  (it "gets individual property correctly"
    (with-org-anki-test-buffer
     "#+begin_anki :deck Test :type Basic :id 12345
  * Front
    Question
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks)))
       (expect (org-anki-blocks--get-property block :deck) :to-equal "Test")
       (expect (org-anki-blocks--get-property block :type) :to-equal "Basic")
       (expect (org-anki-blocks--get-property block :id) :to-equal "12345"))))

  (it "returns nil for non-existent properties"
    (with-org-anki-test-buffer
     "#+begin_anki :deck Test :type Basic
  * Front
    Question
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks)))
       (expect (org-anki-blocks--get-property block :nonexistent) :to-be nil)))))

;;; Field Management Tests

(describe "org-anki-blocks--get-fields"
  (it "extracts Basic card fields"
    (with-org-anki-test-buffer
     "#+begin_anki :deck Test :type Basic
  * Front
    What is the capital of France?
  * Back
    Paris
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (fields (org-anki-blocks--get-fields block)))
       (expect (length fields) :to-be 2)
       (expect (cdr (assoc "Front" fields)) :to-equal "What is the capital of France?")
       (expect (cdr (assoc "Back" fields)) :to-equal "Paris"))))

  (it "extracts Cloze card fields"
    (with-org-anki-test-buffer
     "#+begin_anki :deck Test :type Cloze
  * Text
    The capital of {{c1::France}} is {{c2::Paris}}.
  * Extra
    European geography
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (fields (org-anki-blocks--get-fields block)))
       (expect (length fields) :to-be 2)
       (expect (cdr (assoc "Text" fields)) :to-equal "The capital of {{c1::France}} is {{c2::Paris}}.")
       (expect (cdr (assoc "Extra" fields)) :to-equal "European geography"))))

  (it "handles multi-line field content"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic
  * Front
    What is the capital
    of France?
  * Back
    Paris is the capital
    and largest city
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (fields (org-anki-blocks--get-fields block)))
       (expect (cdr (assoc "Front" fields)) :to-equal "What is the capital\nof France?")
       (expect (cdr (assoc "Back" fields)) :to-equal "Paris is the capital\nand largest city"))))

  (it "handles empty fields"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question
  * Back
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (fields (org-anki-blocks--get-fields block)))
       (expect (length fields) :to-be 2)
       (expect (cdr (assoc "Front" fields)) :to-equal "Question")
       (expect (cdr (assoc "Back" fields)) :to-equal ""))))

  (it "returns empty list when no fields found"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (fields (org-anki-blocks--get-fields block)))
       (expect (length fields) :to-be 0)))))

;;; Block Creation Tests

(describe "org-anki-blocks--create"
  (it "creates Basic card with all properties"
    (with-temp-buffer
      (org-mode)
      (org-anki-blocks--create
       :deck "Programming"
       :type "Basic"
       :id "12345"
       :tags '("elisp" "emacs")
       :hash "testhash"
       :fields '(("Front" . "What is Emacs?")
                 ("Back" . "A text editor")))
      (let ((content (buffer-string)))
        (expect content :to-match "#\\+begin_anki")
        (expect content :to-match ":deck Programming")
        (expect content :to-match ":type Basic")
        (expect content :to-match ":id 12345")
        (expect content :to-match ":tags elisp,emacs")
        (expect content :to-match ":hash testhash")
        (expect content :to-match "\\* Front")
        (expect content :to-match "What is Emacs\\?")
        (expect content :to-match "\\* Back")
        (expect content :to-match "A text editor")
        (expect content :to-match "#\\+end_anki"))))

  (it "creates card with minimal properties"
    (with-temp-buffer
      (org-mode)
      (org-anki-blocks--create
       :deck "Test"
       :type "Basic"
       :fields '(("Front" . "Question")
                 ("Back" . "Answer")))
      (let ((content (buffer-string)))
        (expect content :to-match ":deck Test")
        (expect content :to-match ":type Basic")
        (expect content :not :to-match ":id")
        (expect content :not :to-match ":tags")
        (expect content :not :to-match ":hash"))))

  (it "handles multi-line field content"
    (with-temp-buffer
      (org-mode)
      (org-anki-blocks--create
       :deck "Test"
       :type "Basic"
       :fields '(("Front" . "Line 1\nLine 2\nLine 3")
                 ("Back" . "Single line")))
      (let ((content (buffer-string)))
        (expect content :to-match "Line 1")
        (expect content :to-match "Line 2")
        (expect content :to-match "Line 3")))))

;;; Validation Tests

(describe "org-anki-blocks--validate"
  (it "validates correct Basic card"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question
  * Back
    Answer
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (result (org-anki-blocks--validate block)))
       (expect result :to-be nil))))

  (it "detects missing deck property"
    (with-org-anki-test-buffer
     "#+begin_anki :type Basic
  * Front
    Question
  * Back
    Answer
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (result (org-anki-blocks--validate block)))
       (expect result :to-equal "Missing :deck property"))))

  (it "detects missing type property"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\"
  * Front
    Question
  * Back
    Answer
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (result (org-anki-blocks--validate block)))
       (expect result :to-equal "Missing :type property"))))

  (it "detects missing Front field in Basic card"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic
  * Back
    Answer
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (result (org-anki-blocks--validate block)))
       (expect result :to-equal "Missing Front field"))))

  (it "detects missing Back field in Basic card"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (result (org-anki-blocks--validate block)))
       (expect result :to-equal "Missing Back field"))))

  (it "detects empty Front field"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic
  * Front
  * Back
    Answer
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (result (org-anki-blocks--validate block)))
       (expect result :to-equal "Empty Front field"))))

  (it "detects empty Back field"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question
  * Back
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (result (org-anki-blocks--validate block)))
       (expect result :to-equal "Empty Back field"))))

  (it "validates correct Cloze card"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Cloze
  * Text
    The capital of {{c1::France}} is {{c2::Paris}}.
  * Extra
    Geography
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (result (org-anki-blocks--validate block)))
       (expect result :to-be nil))))

  (it "detects missing Text field in Cloze card"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Cloze
  * Extra
    Information
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (result (org-anki-blocks--validate block)))
       (expect result :to-equal "Missing Text field"))))

  (it "detects empty Text field in Cloze card"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Cloze
  * Text
  * Extra
    Information
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (result (org-anki-blocks--validate block)))
       (expect result :to-equal "Empty Text field"))))

  (it "detects missing cloze deletions"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Cloze
  * Text
    This text has no cloze deletions.
  * Extra
    Information
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (result (org-anki-blocks--validate block)))
       (expect result :to-equal "No cloze deletions found in Text field"))))

  (it "detects no fields defined"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic
#+end_anki"
     (let* ((blocks (org-anki-blocks--find-all))
            (block (car blocks))
            (result (org-anki-blocks--validate block)))
       (expect result :to-equal "No fields defined")))))

;;; Navigation Tests

(describe "org-anki-blocks--goto-next"
  (it "moves to next anki block"
    (with-org-anki-test-buffer
     "* Heading
Some text

#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question 1
#+end_anki

More text

#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question 2
#+end_anki"
     (goto-char (point-min))
     (org-anki-blocks--goto-next)
     (expect (looking-at "#\\+begin_anki") :to-be t)
     (org-anki-blocks--goto-next)
     (expect (looking-at "#\\+begin_anki") :to-be t)))

  (it "stays at point when no more blocks"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question
#+end_anki

No more blocks after this."
     (goto-char (point-max))
     (let ((initial-point (point)))
       (org-anki-blocks--goto-next)
       (expect (point) :to-be initial-point)))))

(describe "org-anki-blocks--goto-previous"
  (it "moves to previous anki block"
    (with-org-anki-test-buffer
     "#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question 1
#+end_anki

Some text

#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question 2
#+end_anki

More text"
     (goto-char (point-max))
     (org-anki-blocks--goto-previous)
     (expect (looking-at "#\\+begin_anki") :to-be t)
     (org-anki-blocks--goto-previous)
     (expect (looking-at "#\\+begin_anki") :to-be t)))

  (it "stays at point when no previous blocks"
    (with-org-anki-test-buffer
     "No blocks before this.

#+begin_anki :deck \"Test\" :type Basic
  * Front
    Question
#+end_anki"
     (goto-char (point-min))
     (let ((initial-point (point)))
       (org-anki-blocks--goto-previous)
       (expect (point) :to-be initial-point)))))

;;; Hash Computation Tests

(describe "org-anki-blocks-sync--compute-hash"
  (it "computes consistent hash for same fields"
    (let ((fields1 '(("Front" . "Question") ("Back" . "Answer")))
          (fields2 '(("Front" . "Question") ("Back" . "Answer"))))
      (expect (org-anki-blocks-sync--compute-hash fields1)
              :to-equal (org-anki-blocks-sync--compute-hash fields2))))

  (it "computes different hash for different fields"
    (let ((fields1 '(("Front" . "Question") ("Back" . "Answer")))
          (fields2 '(("Front" . "Question") ("Back" . "Different"))))
      (expect (org-anki-blocks-sync--compute-hash fields1)
              :not :to-equal (org-anki-blocks-sync--compute-hash fields2))))

  (it "ignores field order"
    (let ((fields1 '(("Front" . "Question") ("Back" . "Answer")))
          (fields2 '(("Back" . "Answer") ("Front" . "Question"))))
      (expect (org-anki-blocks-sync--compute-hash fields1)
              :to-equal (org-anki-blocks-sync--compute-hash fields2))))

  (it "handles empty fields"
    (let ((fields '()))
      (expect (org-anki-blocks-sync--compute-hash fields) :not :to-be nil)))

  (it "handles single field"
    (let ((fields '(("Front" . "Question"))))
      (expect (org-anki-blocks-sync--compute-hash fields) :not :to-be nil))))

(provide 'test-org-anki-blocks)
;;; test-org-anki-blocks.el ends here