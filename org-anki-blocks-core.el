;;; org-anki-blocks-core.el --- Block management for org-anki-blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Myke Bilyanskyy
;; Keywords: outlines, flashcards, anki

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides functions for managing Anki card blocks in org-mode.
;; It handles parsing, creation, modification, and validation of special
;; org blocks used to represent Anki cards.

;;; Code:

(require 'org-element)
(require 'cl-lib)
(require 'subr-x)

;;; Block Finding Functions

(defun org-anki-blocks--find-all ()
  "Find all anki blocks in current buffer."
  (org-element-map (org-element-parse-buffer) 'special-block
    (lambda (block)
      (when (string= (org-element-property :type block) "anki")
        block))))

(defun org-anki-blocks--find-at-point (&optional pos)
  "Find anki block at POS or point."
  (save-excursion
    (goto-char (or pos (point)))
    (let ((element (org-element-at-point)))
      (while (and element 
                  (not (and (eq (org-element-type element) 'special-block)
                           (string= (org-element-property :type element) "anki"))))
        (setq element (org-element-property :parent element)))
      element)))

(defun org-anki-blocks--find-by-id (id)
  "Find anki block with given ID."
  (let ((blocks (org-anki-blocks--find-all)))
    (cl-find-if (lambda (block)
                  (equal (org-anki-blocks--get-property block :id)
                         (if (numberp id) (number-to-string id) id)))
                blocks)))

;;; Property Management

(defun org-anki-blocks--get-property (block property)
  "Get PROPERTY from anki BLOCK header."
  (let* ((begin (org-element-property :begin block))
         (end (org-element-property :contents-begin block))
         (properties (org-anki-blocks--parse-header block)))
    (plist-get properties property)))

(defun org-anki-blocks--get-all-properties (block)
  "Get all properties from anki BLOCK header."
  (org-anki-blocks--parse-header block))

(defun org-anki-blocks--parse-header (block)
  "Parse header properties from anki BLOCK."
  (save-excursion
    (goto-char (org-element-property :begin block))
    (when (looking-at "^#\\+begin_anki\\s-*\\(.*\\)$")
      (let* ((params-str (match-string 1))
             (params nil))
        (when (and params-str (not (string-empty-p params-str)))
          (let ((items (split-string params-str " " t)))
            (while items
              (let ((key (pop items))
                    (value (pop items)))
                (when (and key (string-prefix-p ":" key))
                  (setq params (plist-put params 
                                        (intern key)
                                        value)))))))
        params))))

(defun org-anki-blocks--update-property (block property value)
  "Update PROPERTY with VALUE in anki BLOCK header."
  (org-anki-blocks--update-properties block (list property value)))

(defun org-anki-blocks--update-properties (block &rest properties)
  "Update multiple PROPERTIES in anki BLOCK header."
  (save-excursion
    (goto-char (org-element-property :begin block))
    (when (looking-at "^#\\+begin_anki\\s-*\\(.*\\)$")
      (let* ((match-end (match-end 0))
             (current-params (org-anki-blocks--get-all-properties block))
             (new-params current-params))
        ;; Update properties
        (cl-loop for (key value) on properties by #'cddr
                 do (setq new-params (plist-put new-params key value)))
        ;; Format new parameter string
        (let ((param-strings
               (cl-loop for (key value) on new-params by #'cddr
                        when value
                        collect (format "%s %s" (symbol-name key) value))))
          ;; Delete old line and insert new one
          (delete-region (point) match-end)
          (insert (format "#+begin_anki %s"
                         (mapconcat #'identity param-strings " "))))))))

(defun org-anki-blocks--update (block &rest args)
  "Update BLOCK with properties and/or fields from ARGS.
ARGS can contain :fields followed by an alist, and other properties."
  (let ((fields nil)
        (properties nil))
    ;; Parse arguments
    (while args
      (let ((key (pop args))
            (value (pop args)))
        (cond
         ((eq key :fields)
          (setq fields value))
         (t
          (push value properties)
          (push key properties)))))
    
    ;; Update properties if any
    (when properties
      (apply #'org-anki-blocks--update-properties block (nreverse properties)))
    
    ;; Update fields if provided
    (when fields
      (org-anki-blocks--update-fields block fields))))

(defun org-anki-blocks--update-fields (block fields)
  "Update the fields content of BLOCK with FIELDS alist."
  (save-excursion
    (let ((begin (org-element-property :contents-begin block))
          (end (org-element-property :contents-end block)))
      ;; Delete existing field content
      (delete-region begin end)
      (goto-char begin)
      ;; Insert new fields
      (dolist (field fields)
        (insert (format "  * %s\n" (car field)))
        (let ((lines (split-string (cdr field) "\n")))
          (dolist (line lines)
            (unless (string-empty-p line)
              (insert (format "    %s\n" line)))))))))

;;; Field Management

(defun org-anki-blocks--get-fields (block)
  "Extract field alist from anki BLOCK."
  (let ((begin (org-element-property :contents-begin block))
        (end (org-element-property :contents-end block)))
    (if (and begin end)
        (save-excursion
          (goto-char begin)
          (let ((fields nil)
                (current-field nil))
      (while (< (point) end)
        (cond
         ;; Field header
         ((looking-at "^[ \t]*\\*[ \t]+\\(.+\\)$")
          (when current-field
            (push (cons (car current-field) 
                       (string-trim (cdr current-field)))
                  fields))
          (setq current-field (cons (match-string 1) "")))
         ;; Field content
         ((and current-field (looking-at "^[ \t]*\\(.+\\)$"))
          (let ((content (match-string 1)))
            (setq current-field 
                  (cons (car current-field)
                        (if (string-empty-p (cdr current-field))
                            content
                          (concat (cdr current-field) "\n" content)))))))
        (forward-line))
            ;; Add last field
            (when current-field
              (push (cons (car current-field) 
                         (string-trim (cdr current-field)))
                    fields))
            (nreverse fields)))
      ;; Return empty list if no contents
      nil)))

;;; Block Creation

(defun org-anki-blocks--insert ()
  "Insert a new anki block at point."
  (interactive)
  (let* ((deck (read-string "Deck: " org-anki-blocks-default-deck))
         (type (completing-read "Note type: " 
                               '("Basic" "Basic (and reversed card)" "Cloze")
                               nil nil "Basic"))
         (tags (read-string "Tags (comma-separated): ")))
    (insert (format "#+begin_anki :deck %s :type %s%s\n"
                   deck type
                   (if (string-empty-p tags) ""
                     (format " :tags %s" tags))))
    (cond
     ((string= type "Basic")
      (insert "  * Front\n    \n  * Back\n    \n"))
     ((string= type "Basic (and reversed card)")
      (insert "  * Front\n    \n  * Back\n    \n"))
     ((string= type "Cloze")
      (insert "  * Text\n    \n  * Extra\n    \n")))
    (insert "#+end_anki\n")
    (forward-line -3)
    (end-of-line)))

(defun org-anki-blocks--create (&rest properties)
  "Create a new anki block with PROPERTIES at point.
PROPERTIES should include :deck, :type, and :fields (alist)."
  (let* ((deck (plist-get properties :deck))
         (type (plist-get properties :type))
         (fields (plist-get properties :fields))
         (tags (plist-get properties :tags))
         (id (plist-get properties :id))
         (hash (plist-get properties :hash))
         (params nil))
    ;; Build parameter list in correct order
    (setq params (list (format ":deck %s" deck)
                       (format ":type %s" type)))
    (when id
      (setq params (append params (list (format ":id %s" 
                                               (if (numberp id) (number-to-string id) id))))))
    (when tags
      (setq params (append params (list (format ":tags %s" (mapconcat #'identity tags ","))))))
    (when hash
      (setq params (append params (list (format ":hash %s" hash)))))
    ;; Insert block
    (insert (format "#+begin_anki %s\n" (mapconcat #'identity params " ")))
    ;; Insert fields
    (dolist (field fields)
      (insert (format "  * %s\n" (car field)))
      (let ((lines (split-string (cdr field) "\n")))
        (dolist (line lines)
          (unless (string-empty-p line)
            (insert (format "    %s\n" line))))))
    (insert "#+end_anki\n")))

;;; Block Modification

(defun org-anki-blocks--mark-deleted (block)
  "Mark BLOCK as deleted."
  (org-anki-blocks--update-properties 
   block 
   :deleted "t"
   :delete-time (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun org-anki-blocks--mark-modified (block)
  "Mark BLOCK as modified."
  (org-anki-blocks--update-property block :modified "t"))

(defun org-anki-blocks--delete (block)
  "Delete the entire BLOCK from buffer."
  (let ((begin (org-element-property :begin block))
        (end (org-element-property :end block)))
    (save-excursion
      (delete-region begin end))))

;;; Navigation

(defun org-anki-blocks--goto-next ()
  "Go to next anki block."
  (interactive)
  (let ((pos (point))
        (found nil))
    (save-excursion
      (while (and (not found)
                  (re-search-forward "^#\\+begin_anki" nil t))
        (when (> (match-beginning 0) pos)
          (setq found (match-beginning 0)))))
    (if found
        (goto-char found)
      (message "No more anki blocks"))))

(defun org-anki-blocks--goto-previous ()
  "Go to previous anki block."
  (interactive)
  (let ((pos (point))
        (found nil))
    (save-excursion
      (while (and (not found)
                  (re-search-backward "^#\\+begin_anki" nil t))
        (when (< (match-beginning 0) pos)
          (setq found (match-beginning 0)))))
    (if found
        (goto-char found)
      (message "No previous anki blocks"))))

;;; Validation

(defun org-anki-blocks--validate (block)
  "Validate anki BLOCK structure and properties.
Returns nil if valid, or a string describing the issue."
  (let* ((properties (org-anki-blocks--get-all-properties block))
         (deck (plist-get properties :deck))
         (type (plist-get properties :type))
         (fields (org-anki-blocks--get-fields block)))
    (cond
     ((not deck) "Missing :deck property")
     ((not type) "Missing :type property")
     ((null fields) "No fields defined")
     ((string= type "Basic")
      (cond
       ((not (assoc "Front" fields)) "Missing Front field")
       ((not (assoc "Back" fields)) "Missing Back field")
       ((string-empty-p (cdr (assoc "Front" fields))) "Empty Front field")
       ((string-empty-p (cdr (assoc "Back" fields))) "Empty Back field")))
     ((string= type "Cloze")
      (cond
       ((not (assoc "Text" fields)) "Missing Text field")
       ((string-empty-p (cdr (assoc "Text" fields))) "Empty Text field")
       ((not (string-match "{{c[0-9]+::" (cdr (assoc "Text" fields))))
        "No cloze deletions found in Text field")))
     (t nil))))

(defun org-anki-blocks--validate-all ()
  "Validate all anki blocks in current buffer."
  (interactive)
  (let ((blocks (org-anki-blocks--find-all))
        (invalid 0))
    (dolist (block blocks)
      (when-let ((issue (org-anki-blocks--validate block)))
        (cl-incf invalid)
        (message "Block at %d has issues: %s"
                (org-element-property :begin block)
                issue)))
    (message "Validation complete: %d/%d blocks have issues"
            invalid (length blocks))))

(provide 'org-anki-blocks-core)
;;; org-anki-blocks-core.el ends here
