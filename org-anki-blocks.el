;;; org-anki-blocks.el --- Org-mode integration with Anki using AnkiConnect -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Myke Bilyanskyy
;; Keywords: outlines, flashcards, anki
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; org-anki-blocks provides seamless 2-way synchronization between org-mode
;; and Anki flashcards.  It allows you to create, edit, and manage
;; Anki cards directly from org-mode using special blocks.
;;
;; Features:
;; - Create Anki cards using org-mode blocks
;; - 2-way sync with conflict resolution
;; - Deletion tracking
;; - Bulk operations
;; - No external dependencies (uses AnkiConnect)
;;
;; Usage:
;; (require 'org-anki-blocks)
;; M-x org-anki-blocks-sync-buffer

;;; Code:

(require 'org-anki-blocks-connect)
(require 'org-anki-blocks-core)
(require 'org-anki-blocks-sync)

;;; Customization

(defgroup org-anki-blocks nil
  "Org-mode integration with Anki."
  :group 'org
  :prefix "org-anki-blocks-")

(defcustom org-anki-blocks-default-deck "Default"
  "Default deck for new Anki cards."
  :type 'string
  :group 'org-anki-blocks)

(defcustom org-anki-blocks-default-note-type "Basic"
  "Default note type for new Anki cards."
  :type 'string
  :group 'org-anki-blocks)

(defcustom org-anki-blocks-sync-on-save nil
  "Automatically sync when saving org files with anki blocks."
  :type 'boolean
  :group 'org-anki-blocks)

(defcustom org-anki-blocks-confirm-deletion t
  "Ask for confirmation before deleting cards."
  :type 'boolean
  :group 'org-anki-blocks)

;;; Main Commands

;;;###autoload
(defun org-anki-blocks-sync-block ()
  "Sync the anki block at point."
  (interactive)
  (if-let ((block (org-anki-blocks--find-at-point)))
      (condition-case err
          (progn
            (org-anki-blocks-sync--push-note block)
            (message "Block synced successfully"))
        (error
         (message "Error syncing block: %s" (error-message-string err))))
    (user-error "No anki block at point")))

;;;###autoload
(defun org-anki-blocks-sync-buffer ()
  "Sync all anki blocks in current buffer."
  (interactive)
  (org-anki-blocks-sync--all))

;;;###autoload
(defun org-anki-blocks-pull ()
  "Pull changes from Anki to current buffer."
  (interactive)
  (org-anki-blocks-sync--pull))

;;;###autoload
(defun org-anki-blocks-push ()
  "Push changes from current buffer to Anki."
  (interactive)
  (org-anki-blocks-sync--push))

;;;###autoload
(defun org-anki-blocks-insert ()
  "Insert a new anki block at point."
  (interactive)
  (org-anki-blocks--insert))

;;;###autoload
(defun org-anki-blocks-delete ()
  "Mark the anki block at point for deletion."
  (interactive)
  (if-let ((block (org-anki-blocks--find-at-point)))
      (when (or (not org-anki-confirm-deletion)
                (yes-or-no-p "Mark this block for deletion? "))
        (org-anki-blocks--mark-deleted block)
        (message "Block marked for deletion. Run sync to delete from Anki."))
    (user-error "No anki block at point")))

;;;###autoload
(defun org-anki-blocks-browse ()
  "Browse anki blocks in current buffer."
  (interactive)
  (let ((blocks (org-anki-blocks--find-all)))
    (if blocks
        (let* ((choices
                (mapcar (lambda (block)
                         (let* ((deck (org-anki-blocks--get-property block :deck))
                                (fields (org-anki-blocks--get-fields block))
                                (front (cdr (assoc "Front" fields))))
                           (cons (format "[%s] %s"
                                       (or deck "No deck")
                                       (or (substring front 0 (min 50 (length front)))
                                           "No front"))
                                 block)))
                       blocks))
               (choice (completing-read "Go to block: " choices nil t))
               (block (cdr (assoc choice choices))))
          (when block
            (goto-char (org-element-property :begin block))))
      (message "No anki blocks found in buffer"))))

;;;###autoload
(defun org-anki-blocks-validate-buffer ()
  "Validate all anki blocks in current buffer."
  (interactive)
  (org-anki-blocks--validate-all))

;;;###autoload
(defun org-anki-blocks-import-deck (deck)
  "Import all cards from DECK into current buffer."
  (interactive
   (list (completing-read "Deck to import: " 
                         (org-anki-blocks-connect--deck-names)
                         nil t)))
  (org-anki-blocks-sync-import-deck deck))

;;;###autoload
(defun org-anki-blocks-import-query (query)
  "Import cards matching QUERY into current buffer."
  (interactive "sAnki search query: ")
  (org-anki-blocks-sync-import-query query))

;;; Status Display

(defun org-anki-blocks-status ()
  "Display sync status for current buffer."
  (interactive)
  (let* ((blocks (org-anki-blocks--find-all))
         (total (length blocks))
         (synced 0)
         (modified 0)
         (new 0)
         (deleted 0))
    (dolist (block blocks)
      (cond
       ((org-anki-blocks--get-property block :deleted)
        (cl-incf deleted))
       ((not (org-anki-blocks--get-property block :id))
        (cl-incf new))
       ((org-anki-blocks--get-property block :modified)
        (cl-incf modified))
       (t
        (cl-incf synced))))
    (message "Anki blocks: %d total, %d synced, %d modified, %d new, %d deleted"
             total synced modified new deleted)))

;;; Minor Mode

(defvar org-anki-blocks-mode-map
  (make-sparse-keymap)
  "Keymap for org-anki-blocks-mode.")

;;;###autoload
(define-minor-mode org-anki-blocks-mode
  "Minor mode for org-anki integration."
  :lighter " OrgAnkiBlocks"
  :keymap org-anki-blocks-mode-map
  (if org-anki-blocks-mode
      (when org-anki-blocks-sync-on-save
        (org-anki-blocks-sync--enable-auto-sync))
    (org-anki-blocks-sync--disable-auto-sync)))

;;; Setup

;;;###autoload
(defun org-anki-blocks-setup ()
  "Setup org-anki in current buffer."
  (interactive)
  (org-anki-blocks-mode 1))

;;; Utilities

(defun org-anki-blocks-test-connection ()
  "Test connection to AnkiConnect."
  (interactive)
  (org-anki-blocks-connect--test))

(defun org-anki-blocks-list-decks ()
  "List all available Anki decks."
  (interactive)
  (let ((decks (org-anki-blocks-connect--deck-names)))
    (if decks
        (message "Available decks: %s" (string-join decks ", "))
      (message "No decks found or connection failed"))))

;;; Hooks

(defun org-anki-blocks--maybe-enable-mode ()
  "Enable org-anki-blocks-mode if buffer contains anki blocks."
  (when (and (derived-mode-p 'org-mode)
             (require 'org-anki-blocks-core nil t)
             (fboundp 'org-anki-blocks--find-all)
             (org-anki-blocks--find-all))
    (org-anki-blocks-mode 1)))

;; Add hook after package is loaded
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'org-anki-blocks--maybe-enable-mode))

(provide 'org-anki-blocks)
;;; org-anki-blocks.el ends here