;;; org-anki-blocks-sync.el --- 2-way synchronization engine for org-anki-blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Myke Bilyanskyy
;; Keywords: outlines, flashcards, anki

;;; Commentary:

;; This library provides 2-way synchronization between org-mode blocks
;; and Anki, including conflict resolution and deletion tracking.

;;; Code:

(require 'org-anki-blocks-connect)
(require 'cl-lib)
(require 'subr-x)

;;; Customization

(defgroup org-anki-blocks-sync nil
  "Synchronization settings for org-anki-blocks."
  :group 'org-anki-blocks)

(defcustom org-anki-blocks-sync-conflict-strategy 'ask
  "Strategy for handling sync conflicts.
Possible values:
- `ask': Prompt user to resolve conflicts
- `local-wins': Always keep local version
- `remote-wins': Always keep remote version
- `newer-wins': Keep the newer version based on timestamps"
  :type '(choice (const :tag "Ask user" ask)
                 (const :tag "Local wins" local-wins)
                 (const :tag "Remote wins" remote-wins)
                 (const :tag "Newer wins" newer-wins))
  :group 'org-anki-blocks-sync)

(defcustom org-anki-blocks-sync-on-save nil
  "Automatically sync when saving org files containing anki blocks."
  :type 'boolean
  :group 'org-anki-blocks-sync)

(defcustom org-anki-blocks-sync-delete-after-days 30
  "Number of days to keep deleted blocks before purging.
Set to nil to never auto-purge."
  :type '(choice integer (const nil))
  :group 'org-anki-blocks-sync)

;;; Internal Variables

(defvar org-anki-blocks-sync--in-progress nil
  "Non-nil when sync is in progress.")

(defvar org-anki-blocks-sync--conflicts nil
  "List of conflicts found during sync.")

;;; Hash Computation

(defun org-anki-blocks-sync--compute-hash (fields)
  "Compute a hash of FIELDS content for change detection."
  (secure-hash 'sha256
               (mapconcat (lambda (field)
                           (format "%s:%s" (car field) (cdr field)))
                         (sort (copy-sequence fields)
                               (lambda (a b) (string< (car a) (car b))))
                         "\n")))

;;; Change Detection

(defun org-anki-blocks-sync--note-modified-p (block remote-info)
  "Check if BLOCK has been modified compared to REMOTE-INFO."
  (let* ((local-fields (org-anki-blocks--get-fields block))
         (local-hash (org-anki-blocks-sync--compute-hash local-fields))
         (stored-hash (org-anki-blocks--get-property block :hash)))
    (or (not stored-hash)
        (not (string= local-hash stored-hash)))))

(defun org-anki-blocks-sync--remote-modified-p (block remote-info)
  "Check if REMOTE-INFO differs from BLOCK's last known state."
  (let* ((remote-fields (org-anki-blocks-sync--extract-fields remote-info))
         (remote-hash (org-anki-blocks-sync--compute-hash remote-fields))
         (stored-hash (org-anki-blocks--get-property block :hash)))
    (or (not stored-hash)
        (not (string= remote-hash stored-hash)))))

(defun org-anki-blocks-sync--extract-fields (note-info)
  "Extract fields from NOTE-INFO returned by AnkiConnect."
  (let ((fields (alist-get 'fields note-info)))
    (mapcar (lambda (field)
              (cons (car field)
                    (alist-get 'value (cdr field))))
            fields)))

;;; Conflict Resolution

(defun org-anki-blocks-sync--create-conflict (block local-fields remote-fields)
  "Create a conflict record for BLOCK with LOCAL-FIELDS and REMOTE-FIELDS."
  `((block . ,block)
    (local . ,local-fields)
    (remote . ,remote-fields)
    (id . ,(org-anki-blocks--get-property block :id))))

(defun org-anki-blocks-sync--resolve-conflict (conflict)
  "Resolve a sync CONFLICT based on configured strategy."
  (let ((strategy org-anki-blocks-sync-conflict-strategy))
    (cond
     ((eq strategy 'local-wins) 'local)
     ((eq strategy 'remote-wins) 'remote)
     ((eq strategy 'newer-wins)
      (org-anki-blocks-sync--resolve-by-timestamp conflict))
     ((eq strategy 'ask)
      (org-anki-blocks-sync--ask-user-resolution conflict))
     (t (error "Unknown conflict strategy: %s" strategy)))))

(defun org-anki-blocks-sync--ask-user-resolution (conflict)
  "Interactively ask user to resolve CONFLICT."
  (let* ((local (alist-get 'local conflict))
         (remote (alist-get 'remote conflict))
         (id (alist-get 'id conflict))
         (buffer (get-buffer-create "*Org Anki Conflict*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Conflict for note ID: %s\n\n" id))
      (insert "=== LOCAL VERSION ===\n")
      (dolist (field local)
        (insert (format "%s: %s\n" (car field) (cdr field))))
      (insert "\n=== REMOTE VERSION ===\n")
      (dolist (field remote)
        (insert (format "%s: %s\n" (car field) (cdr field))))
      (insert "\n\nChoose: (l)ocal, (r)emote, (m)erge, (s)kip: "))
    (pop-to-buffer buffer)
    (let ((choice (read-char)))
      (kill-buffer buffer)
      (cond
       ((eq choice ?l) 'local)
       ((eq choice ?r) 'remote)
       ((eq choice ?m) (org-anki-blocks-sync--merge-interactive conflict))
       ((eq choice ?s) 'skip)
       (t (error "Invalid choice"))))))

(defun org-anki-blocks-sync--merge-interactive (conflict)
  "Interactively merge CONFLICT."
  ;; Simple implementation - could be enhanced with ediff
  (let* ((local (alist-get 'local conflict))
         (remote (alist-get 'remote conflict))
         (merged (copy-sequence local)))
    (dolist (field remote)
      (when (yes-or-no-p (format "Use remote value for %s? " (car field)))
        (setf (alist-get (car field) merged nil nil #'string=)
              (cdr field))))
    `(merged . ,merged)))

;;; Sync Operations

(defun org-anki-blocks-sync--pull-note (remote-info)
  "Pull a note from REMOTE-INFO into the current buffer."
  (let* ((id (alist-get 'noteId remote-info))
         (deck (alist-get 'deckName remote-info))
         (model (alist-get 'modelName remote-info))
         (fields (org-anki-blocks-sync--extract-fields remote-info))
         (tags (alist-get 'tags remote-info))
         (block (org-anki-blocks--find-by-id id)))
    (if block
        ;; Update existing block
        (org-anki-blocks--update block
                               :fields fields
                               :deck deck
                               :type model
                               :tags tags
                               :hash (org-anki-blocks-sync--compute-hash fields))
      ;; Create new block
      (org-anki-blocks--create :id id
                             :deck deck
                             :type model
                             :fields fields
                             :tags tags
                             :hash (org-anki-blocks-sync--compute-hash fields)))))

(defun org-anki-blocks-sync--push-note (block)
  "Push BLOCK to Anki."
  (let* ((id (org-anki-blocks--get-property block :id))
         (deck (org-anki-blocks--get-property block :deck))
         (model (org-anki-blocks--get-property block :type))
         (fields (org-anki-blocks--get-fields block))
         (tags (org-anki-blocks--get-property block :tags))
         (deleted (org-anki-blocks--get-property block :deleted)))
    (cond
     ;; Handle deletion
     (deleted
      (when id
        (org-anki-blocks-connect--delete-notes (list id))
        (org-anki-blocks--remove block)))
     ;; Create new note
     ((not id)
      (let ((new-id (org-anki-blocks-connect--add-note deck model fields tags)))
        (org-anki-blocks--update block
                               :id (if (numberp new-id) 
                                       (number-to-string new-id)
                                     new-id)
                               :hash (org-anki-blocks-sync--compute-hash fields))))
     ;; Update existing note
     (t
      (org-anki-blocks-connect--update-note id fields)
      (org-anki-blocks--update block
                             :hash (org-anki-blocks-sync--compute-hash fields))))))

;;; Main Sync Functions

(defun org-anki-blocks-sync-pull (&optional deck)
  "Pull changes from Anki for cards that exist in current buffer.
If DECK is specified, only sync cards from that deck."
  (interactive)
  (let* ((existing-blocks (org-anki-blocks--find-all))
         (existing-ids (cl-remove-if-not 
                       #'identity
                       (mapcar (lambda (block)
                                (org-anki-blocks--get-property block :id))
                              existing-blocks)))
         (conflicts nil)
         (updated 0)
         (skipped 0))
    (if (not existing-ids)
        (message "No cards with IDs found in buffer")
      (let ((notes-info (org-anki-blocks-connect--notes-info-with-deck existing-ids)))
        (dolist (remote-info notes-info)
          (let* ((id (alist-get 'noteId remote-info))
                 (block (org-anki-blocks--find-by-id id)))
            (when block
              (cond
               ;; Check for conflicts
               ((and (org-anki-blocks-sync--note-modified-p block remote-info)
                     (org-anki-blocks-sync--remote-modified-p block remote-info))
                (push (org-anki-blocks-sync--create-conflict
                       block
                       (org-anki-blocks--get-fields block)
                       (org-anki-blocks-sync--extract-fields remote-info))
                      conflicts))
               ;; Remote changed, local unchanged
               ((org-anki-blocks-sync--remote-modified-p block remote-info)
                (org-anki-blocks-sync--pull-note remote-info)
                (cl-incf updated))
               ;; No changes
               (t (cl-incf skipped))))))
        ;; Handle conflicts
        (when conflicts
          (org-anki-blocks-sync--process-conflicts conflicts))
        (message "Pull complete: %d updated, %d skipped, %d conflicts"
                 updated skipped (length conflicts))))))

(defun org-anki-blocks-sync-push ()
  "Push local changes to Anki."
  (interactive)
  (let ((blocks (org-anki-blocks--find-all))
        (pushed 0)
        (errors 0))
    (dolist (block blocks)
      (condition-case err
          (when (or (not (org-anki-blocks--get-property block :id))
                    (org-anki-blocks--get-property block :modified)
                    (org-anki-blocks--get-property block :deleted))
            (org-anki-blocks-sync--push-note block)
            (cl-incf pushed))
        (error
         (cl-incf errors)
         (message "Error pushing note: %s" (error-message-string err)))))
    (message "Push complete: %d notes pushed, %d errors" pushed errors)))

(defun org-anki-blocks-sync-all ()
  "Perform full 2-way sync."
  (interactive)
  (when org-anki-blocks-sync--in-progress
    (error "Sync already in progress"))
  (let ((org-anki-blocks-sync--in-progress t))
    (unwind-protect
        (progn
          (message "Starting 2-way sync...")
          (org-anki-blocks-sync-pull)
          (org-anki-blocks-sync-push)
          (message "2-way sync complete"))
      (setq org-anki-blocks-sync--in-progress nil))))

;;; Conflict Processing

(defun org-anki-blocks-sync--process-conflicts (conflicts)
  "Process list of CONFLICTS."
  (dolist (conflict conflicts)
    (let ((resolution (org-anki-blocks-sync--resolve-conflict conflict)))
      (cond
       ((eq resolution 'local)
        (org-anki-blocks-sync--push-note (alist-get 'block conflict)))
       ((eq resolution 'remote)
        (org-anki-blocks-sync--pull-note
         `((noteId . ,(alist-get 'id conflict))
           (fields . ,(org-anki-blocks-sync--fields-to-remote-format
                      (alist-get 'remote conflict))))))
       ((and (consp resolution) (eq (car resolution) 'merged))
        (let ((block (alist-get 'block conflict))
              (merged-fields (cdr resolution)))
          (org-anki-blocks--update block :fields merged-fields)
          (org-anki-blocks-sync--push-note block)))
       ((eq resolution 'skip)
        (message "Skipped conflict for note %s" (alist-get 'id conflict)))))))

(defun org-anki-blocks-sync--fields-to-remote-format (fields)
  "Convert FIELDS to remote format for pull operations."
  (mapcar (lambda (field)
            (cons (car field)
                  `((value . ,(cdr field))
                    (order . 0))))
          fields))

;;; Auto-sync

(defun org-anki-blocks-sync--maybe-auto-sync ()
  "Auto-sync if enabled and appropriate."
  (when (and org-anki-blocks-sync-on-save
             (org-anki-blocks--find-all)
             (not org-anki-blocks-sync--in-progress))
    (org-anki-blocks-sync-all)))

(defun org-anki-blocks-sync-enable-auto-sync ()
  "Enable automatic sync on save."
  (add-hook 'after-save-hook #'org-anki-blocks-sync--maybe-auto-sync nil t))

(defun org-anki-blocks-sync-disable-auto-sync ()
  "Disable automatic sync on save."
  (remove-hook 'after-save-hook #'org-anki-blocks-sync--maybe-auto-sync t))

;;; Deletion Management

(defun org-anki-blocks-sync-mark-deleted (block)
  "Mark BLOCK as deleted for next sync."
  (org-anki-blocks--update block :deleted t))

(defun org-anki-blocks-sync-purge-deleted ()
  "Purge blocks marked as deleted after configured time period."
  (interactive)
  (when org-anki-blocks-sync-delete-after-days
    (let ((cutoff (- (float-time)
                    (* org-anki-blocks-sync-delete-after-days 24 60 60)))
          (purged 0))
      (dolist (block (org-anki-blocks--find-all))
        (when-let* ((deleted (org-anki-blocks--get-property block :deleted))
                    (delete-time (org-anki-blocks--get-property block :delete-time))
                    ((< delete-time cutoff)))
          (org-anki-blocks--remove block)
          (cl-incf purged)))
      (message "Purged %d deleted blocks" purged))))

;;; Import Functions

(defun org-anki-blocks-sync-import-deck (deck)
  "Import all cards from DECK into current buffer.
This will create new org-anki blocks for cards that don't already exist."
  (interactive
   (list (completing-read "Deck to import: " 
                         (org-anki-blocks-connect--deck-names)
                         nil t)))
  (let* ((query (format "deck:%s" deck))
         (note-ids (org-anki-blocks-connect--find-notes query))
         (existing-blocks (org-anki-blocks--find-all))
         (existing-ids (mapcar (lambda (block)
                                (org-anki-blocks--get-property block :id))
                              existing-blocks))
         (imported 0)
         (skipped 0))
    (if (not note-ids)
        (message "No cards found in deck '%s'" deck)
      (message "Found %d cards in deck '%s'" (length note-ids) deck)
      (let ((notes-info (org-anki-blocks-connect--notes-info-with-deck note-ids)))
        (dolist (remote-info notes-info)
          (let ((id (number-to-string (alist-get 'noteId remote-info))))
            (if (member id existing-ids)
                (cl-incf skipped)
              ;; Create new block
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (org-anki-blocks-sync--pull-note remote-info)
              (cl-incf imported))))
        (message "Import complete: %d imported, %d already existed"
                 imported skipped)))))

(defun org-anki-blocks-sync-import-query (query)
  "Import all cards matching QUERY into current buffer.
QUERY uses Anki's search syntax (e.g., 'tag:vocabulary deck:English')."
  (interactive "sAnki search query: ")
  (let* ((note-ids (org-anki-blocks-connect--find-notes query))
         (existing-blocks (org-anki-blocks--find-all))
         (existing-ids (mapcar (lambda (block)
                                (org-anki-blocks--get-property block :id))
                              existing-blocks))
         (imported 0)
         (skipped 0))
    (if (not note-ids)
        (message "No cards found matching query '%s'" query)
      (message "Found %d cards matching query" (length note-ids))
      (let ((notes-info (org-anki-blocks-connect--notes-info-with-deck note-ids)))
        (dolist (remote-info notes-info)
          (let ((id (number-to-string (alist-get 'noteId remote-info))))
            (if (member id existing-ids)
                (cl-incf skipped)
              ;; Create new block
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (org-anki-blocks-sync--pull-note remote-info)
              (cl-incf imported))))
        (message "Import complete: %d imported, %d already existed"
                 imported skipped)))))

(provide 'org-anki-blocks-sync)
;;; org-anki-blocks-sync.el ends here