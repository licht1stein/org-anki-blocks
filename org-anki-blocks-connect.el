;;; org-anki-blocks-connect.el --- AnkiConnect API client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Myke Bilyanskyy
;; Keywords: outlines, flashcards, anki
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library provides a direct interface to AnkiConnect API
;; without external dependencies.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)

;;; Customization

(defgroup org-anki-blocks-connect nil
  "AnkiConnect API client."
  :group 'org-anki-blocks)

(defcustom org-anki-blocks-connect-url "http://localhost:8765"
  "URL for AnkiConnect API endpoint."
  :type 'string
  :group 'org-anki-blocks-connect)

(defcustom org-anki-blocks-connect-timeout 10
  "Timeout in seconds for AnkiConnect requests."
  :type 'integer
  :group 'org-anki-blocks-connect)

;;; Core API

(defun org-anki-blocks-connect--request (action &optional params)
  "Send request to AnkiConnect with ACTION and optional PARAMS.
Returns the result field from the response, or signals an error."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (json-encode
                          `((action . ,action)
                            (version . 6)
                            ,@(when params `((params . ,params))))))
        (json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        response)
    (with-current-buffer
        (url-retrieve-synchronously org-anki-blocks-connect-url nil nil org-anki-blocks-connect-timeout)
      (goto-char (point-min))
      (re-search-forward "^$" nil t)
      (setq response (json-read))
      (kill-buffer))
    (if-let ((error (alist-get 'error response)))
        (error "AnkiConnect error: %s" error)
      (alist-get 'result response))))

(defun org-anki-blocks-connect--test ()
  "Test connection to AnkiConnect."
  (interactive)
  (condition-case err
      (progn
        (org-anki-blocks-connect--request "version")
        (message "AnkiConnect is running and accessible"))
    (error
     (message "AnkiConnect connection failed: %s" (error-message-string err)))))

;;; Deck Operations

(defun org-anki-blocks-connect--deck-names ()
  "Get list of all deck names."
  (org-anki-blocks-connect--request "deckNames"))

(defun org-anki-blocks-connect--create-deck (deck)
  "Create a new DECK."
  (org-anki-blocks-connect--request "createDeck" `((deck . ,deck))))

;;; Note Operations

(defun org-anki-blocks-connect--add-note (deck model fields &optional tags)
  "Add a new note to DECK with MODEL and FIELDS.
FIELDS should be an alist like ((\"Front\" . \"content\") (\"Back\" . \"content\")).
Returns the new note ID."
  (let ((note `((deckName . ,deck)
                (modelName . ,model)
                (fields . ,(map-into fields 'hash-table))
                ,@(when tags `((tags . ,tags))))))
    (org-anki-blocks-connect--request "addNote" `((note . ,note)))))

(defun org-anki-blocks-connect--update-note (id fields)
  "Update note with ID with new FIELDS."
  (let ((note `((id . ,(if (stringp id) (string-to-number id) id))
                (fields . ,(map-into fields 'hash-table)))))
    (org-anki-blocks-connect--request "updateNoteFields" `((note . ,note)))))

(defun org-anki-blocks-connect--delete-notes (ids)
  "Delete notes with given IDS (list of note IDs)."
  (org-anki-blocks-connect--request "deleteNotes" `((notes . ,ids))))

;;; Note Queries

(defun org-anki-blocks-connect--find-notes (query)
  "Find notes matching QUERY.
Returns a list of note IDs."
  (org-anki-blocks-connect--request "findNotes" `((query . ,query))))

(defun org-anki-blocks-connect--notes-info (ids)
  "Get detailed information for notes with IDS.
Returns a list of note info alists."
  (org-anki-blocks-connect--request "notesInfo" 
                            `((notes . ,(mapcar (lambda (id)
                                                 (if (stringp id)
                                                     (string-to-number id)
                                                   id))
                                               ids)))))

(defun org-anki-blocks-connect--cards-info (card-ids)
  "Get detailed information for cards with CARD-IDS.
Returns a list of card info alists including deck names."
  (org-anki-blocks-connect--request "cardsInfo" 
                            `((cards . ,(mapcar (lambda (id)
                                                 (if (stringp id)
                                                     (string-to-number id)
                                                   id))
                                               card-ids)))))

(defun org-anki-blocks-connect--find-notes-in-deck (deck)
  "Find all notes in DECK."
  (org-anki-blocks-connect--find-notes (format "deck:%s" deck)))

;;; Batch Operations

(defun org-anki-blocks-connect--multi (&rest actions)
  "Execute multiple ACTIONS in a single request.
Each action should be a plist with :action and :params keys."
  (let ((requests (mapcar (lambda (act)
                           `((action . ,(plist-get act :action))
                             ,@(when-let ((params (plist-get act :params)))
                                 `((params . ,params)))))
                         actions)))
    (org-anki-blocks-connect--request "multi" `((actions . ,requests)))))

;;; Media Operations

(defun org-anki-blocks-connect--store-media-file (filename data)
  "Store media file with FILENAME and base64-encoded DATA."
  (org-anki-blocks-connect--request "storeMediaFile"
                            `((filename . ,filename)
                              (data . ,data))))

;;; Card Operations

(defun org-anki-blocks-connect--cards-to-notes (card-ids)
  "Convert CARD-IDS to their corresponding note IDs."
  (org-anki-blocks-connect--request "cardsToNotes" `((cards . ,card-ids))))

(defun org-anki-blocks-connect--suspend-cards (card-ids)
  "Suspend cards with CARD-IDS."
  (org-anki-blocks-connect--request "suspend" `((cards . ,card-ids))))

(defun org-anki-blocks-connect--unsuspend-cards (card-ids)
  "Unsuspend cards with CARD-IDS."
  (org-anki-blocks-connect--request "unsuspend" `((cards . ,card-ids))))

;;; GUI Operations

(defun org-anki-blocks-connect--gui-browse (query)
  "Open Anki browser with QUERY."
  (org-anki-blocks-connect--request "guiBrowse" `((query . ,query))))

(defun org-anki-blocks-connect--gui-add-cards (&optional deck)
  "Open Add Cards dialog, optionally with DECK preselected."
  (org-anki-blocks-connect--request "guiAddCards"
                            (when deck `((note . ((deckName . ,deck)))))))

;;; Utility Functions

(defun org-anki-blocks-connect--note-exists-p (id)
  "Check if note with ID exists."
  (condition-case nil
      (progn
        (org-anki-blocks-connect--notes-info (list id))
        t)
    (error nil)))

(defun org-anki-blocks-connect--get-note-fields (id)
  "Get fields of note with ID as an alist."
  (when-let* ((info (car (org-anki-blocks-connect--notes-info (list id))))
              (fields (alist-get 'fields info)))
    (mapcar (lambda (field)
              (cons (car field)
                    (alist-get 'value (cdr field))))
            fields)))

(defun org-anki-blocks-connect--notes-info-with-deck (ids)
  "Get detailed information for notes with IDS, including deck names.
Combines notesInfo and cardsInfo to get complete information."
  (let ((notes-info (org-anki-blocks-connect--notes-info ids))
        (cards-info (org-anki-blocks-connect--cards-info ids)))
    ;; Merge deck information into notes info
    (mapcar (lambda (note-info)
              (let* ((note-id (alist-get 'noteId note-info))
                     (card-info (cl-find-if 
                                (lambda (card) 
                                  (equal (alist-get 'note card) note-id))
                                cards-info))
                     (deck-name (when card-info
                                 (alist-get 'deckName card-info))))
                ;; Add deckName to note info
                (if deck-name
                    (cons (cons 'deckName deck-name) note-info)
                  note-info)))
            notes-info)))

(provide 'org-anki-blocks-connect)
;;; org-anki-blocks-connect.el ends here