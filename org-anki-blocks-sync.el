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
(require 'seq)

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
  "Compute a short hash of FIELDS content for change detection."
  (let ((content (mapconcat (lambda (field)
                             (format "%s:%s" (car field) (cdr field)))
                           (sort (copy-sequence fields)
                                 (lambda (a b) (string< (car a) (car b))))
                           "\n")))
    ;; Use first 8 characters of SHA256 for a short, readable hash
    (substring (secure-hash 'sha256 content) 0 8)))

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
  "Interactively ask user to resolve CONFLICT with improved UI."
  (let* ((local (alist-get 'local conflict))
         (remote (alist-get 'remote conflict))
         (id (alist-get 'id conflict))
         (buffer (get-buffer-create "*Org Anki Conflict Resolution*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (org-anki-blocks-sync--setup-conflict-buffer local remote id)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    
    (let ((window (display-buffer buffer '(display-buffer-pop-up-window . nil))))
      (select-window window)
      (fit-window-to-buffer window)
      
      (unwind-protect
          (org-anki-blocks-sync--handle-conflict-input conflict buffer)
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun org-anki-blocks-sync--setup-conflict-buffer (local remote id)
  "Setup the conflict resolution buffer with LOCAL and REMOTE fields and ID."
  (insert (propertize "╔═══════════════════════════════════════════════════════════════════════════════╗\n" 'face 'bold))
  (insert (propertize "║                           ANKI SYNC CONFLICT                                 ║\n" 'face 'bold))
  (insert (propertize "╚═══════════════════════════════════════════════════════════════════════════════╝\n" 'face 'bold))
  (insert "\n")
  (insert (propertize (format "Note ID: %s\n\n" id) 'face 'italic))
  
  ;; Show field-by-field comparison
  (let ((all-fields (delete-dups (append (mapcar #'car local) (mapcar #'car remote)))))
    (dolist (field-name all-fields)
      (let ((local-value (cdr (assoc field-name local)))
            (remote-value (cdr (assoc field-name remote))))
        (insert (propertize (format "Field: %s\n" field-name) 'face 'bold))
        (insert (propertize "┌─ LOCAL ──────────────────────────────────────────────────────────────────┐\n" 'face 'diff-header))
        (if local-value
            (insert (propertize (format "│ %s\n" (org-anki-blocks-sync--format-field-content local-value)) 
                               'face (if (string= local-value remote-value) 'default 'diff-removed)))
          (insert (propertize "│ [MISSING]\n" 'face 'diff-removed)))
        (insert (propertize "├─ REMOTE ────────────────────────────────────────────────────────────────┤\n" 'face 'diff-header))
        (if remote-value
            (insert (propertize (format "│ %s\n" (org-anki-blocks-sync--format-field-content remote-value))
                               'face (if (string= local-value remote-value) 'default 'diff-added)))
          (insert (propertize "│ [MISSING]\n" 'face 'diff-added)))
        (insert (propertize "└──────────────────────────────────────────────────────────────────────────┘\n" 'face 'diff-header))
        (insert "\n"))))
  
  (insert (propertize "═══════════════════════════════════════════════════════════════════════════════\n" 'face 'bold))
  (insert (propertize "RESOLUTION OPTIONS:\n" 'face 'bold))
  (insert (propertize "  [l] Use LOCAL version    " 'face 'success) 
          (propertize "[r] Use REMOTE version\n" 'face 'warning))
  (insert (propertize "  [m] Merge interactively  " 'face 'info) 
          (propertize "[s] Skip this conflict\n" 'face 'shadow))
  (insert (propertize "  [q] Quit conflict resolution\n" 'face 'error))
  (insert (propertize "═══════════════════════════════════════════════════════════════════════════════\n" 'face 'bold))
  (insert "\nPress a key to choose resolution..."))

(defun org-anki-blocks-sync--format-field-content (content)
  "Format CONTENT for display, handling long text and newlines."
  (let ((lines (split-string content "\n")))
    (if (> (length lines) 3)
        (concat (mapconcat #'identity (seq-take lines 2) "\n│ ") 
                (format "\n│ ... (%d more lines)" (- (length lines) 2)))
      (mapconcat #'identity lines "\n│ "))))

(defun org-anki-blocks-sync--handle-conflict-input (conflict buffer)
  "Handle user input for CONFLICT resolution in BUFFER."
  (let ((choice nil)
        (message-displayed nil))
    (while (not choice)
      (let ((key (read-key (if message-displayed 
                              "Invalid choice! Press [l]ocal, [r]emote, [m]erge, [s]kip, or [q]uit: "
                            "Choose resolution: "))))
        (cond
         ((eq key ?l) (setq choice 'local))
         ((eq key ?r) (setq choice 'remote))
         ((eq key ?m) (setq choice (org-anki-blocks-sync--merge-interactive conflict)))
         ((eq key ?s) (setq choice 'skip))
         ((eq key ?q) (setq choice 'quit))
         (t (setq message-displayed t)))))
    choice))

(defun org-anki-blocks-sync--merge-interactive (conflict)
  "Interactively merge CONFLICT with field-by-field selection."
  (let* ((local (alist-get 'local conflict))
         (remote (alist-get 'remote conflict))
         (all-fields (delete-dups (append (mapcar #'car local) (mapcar #'car remote))))
         (merged nil)
         (buffer (get-buffer-create "*Org Anki Field Merge*")))
    
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    
    (dolist (field-name all-fields)
      (let* ((local-value (cdr (assoc field-name local)))
             (remote-value (cdr (assoc field-name remote)))
             (choice nil))
        
        (with-current-buffer buffer
          (erase-buffer)
          (insert (propertize "╔═══════════════════════════════════════════════════════════════════════════════╗\n" 'face 'bold))
          (insert (propertize "║                            FIELD MERGE                                       ║\n" 'face 'bold))
          (insert (propertize "╚═══════════════════════════════════════════════════════════════════════════════╝\n" 'face 'bold))
          (insert "\n")
          (insert (propertize (format "Field: %s\n\n" field-name) 'face 'bold))
          
          (insert (propertize "LOCAL VERSION:\n" 'face 'diff-removed))
          (insert (format "%s\n\n" (or local-value "[MISSING]")))
          
          (insert (propertize "REMOTE VERSION:\n" 'face 'diff-added))
          (insert (format "%s\n\n" (or remote-value "[MISSING]")))
          
          (insert (propertize "Choose version:\n" 'face 'bold))
          (insert (propertize "  [l] Use LOCAL    [r] Use REMOTE    [e] Edit manually\n" 'face 'info))
          (insert (propertize "  [s] Skip field   [q] Cancel merge\n" 'face 'shadow))
          (setq buffer-read-only t))
        
        (let ((window (display-buffer buffer '(display-buffer-pop-up-window . nil))))
          (select-window window)
          (fit-window-to-buffer window)
          
          (while (not choice)
            (let ((key (read-key "Choose field value: ")))
              (cond
               ((eq key ?l) (when local-value
                              (push (cons field-name local-value) merged)
                              (setq choice 'done)))
               ((eq key ?r) (when remote-value
                              (push (cons field-name remote-value) merged)
                              (setq choice 'done)))
               ((eq key ?e) (let ((custom-value (read-string (format "Enter value for %s: " field-name)
                                                            (or local-value remote-value ""))))
                              (push (cons field-name custom-value) merged)
                              (setq choice 'done)))
               ((eq key ?s) (setq choice 'skip))
               ((eq key ?q) (setq choice 'cancel))))))
        
        (when (eq choice 'cancel)
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (error "Merge cancelled by user"))))
    
    (when (buffer-live-p buffer) (kill-buffer buffer))
    `(merged . ,(nreverse merged))))

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
  "Push BLOCK to Anki.
Returns t if successful, nil if failed."
  (let* ((id (org-anki-blocks--get-property block :id))
         (deck (org-anki-blocks--get-property block :deck))
         (model (org-anki-blocks--get-property block :type))
         (fields (org-anki-blocks--get-fields block))
         (tags (org-anki-blocks--get-property block :tags))
         (deleted (org-anki-blocks--get-property block :deleted)))
    ;; Validate required properties
    (unless deck
      (error "Missing deck property in block"))
    (unless model
      (error "Missing type property in block"))
    (unless fields
      (error "No fields found in block"))
    (cond
     ;; Handle deletion
     (deleted
      (when id
        (org-anki-blocks-connect--delete-notes (list id))
        (org-anki-blocks--remove block)
        t))
     ;; Create new note
     ((not id)
      (condition-case err
          (let ((new-id (org-anki-blocks-connect--add-note deck model fields tags)))
            (org-anki-blocks--update block
                                   :id (format "%s" new-id)
                                   :hash (org-anki-blocks-sync--compute-hash fields))
            t)
        (error 
         (if (string-match-p "duplicate" (error-message-string err))
             (progn
               (org-anki-blocks-sync--handle-duplicate block deck model fields tags)
               ;; Re-parse the block to get the updated properties
               (goto-char (org-element-property :begin block))
               (let* ((updated-block (org-element-at-point))
                      (new-id (org-anki-blocks--get-property updated-block :id)))
                 (if new-id
                     (progn
                       (message "Block synced successfully after duplicate resolution (ID: %s)" new-id)
                       t)
                   (progn
                     (message "Duplicate resolution skipped or failed - block not synced")
                     nil))))
           (signal (car err) (cdr err))))))
     ;; Update existing note
     (t
      (org-anki-blocks-connect--update-note id fields)
      (org-anki-blocks--update block
                             :hash (org-anki-blocks-sync--compute-hash fields))
      t))))

;;; Duplicate Handling

(defun org-anki-blocks-sync--handle-duplicate (block deck model fields tags)
  "Handle duplicate note creation for BLOCK with DECK, MODEL, FIELDS, and TAGS."
  (let* ((front-field (or (cdr (assoc "Front" fields))
                         (cdr (assoc "Text" fields))
                         (cdr (car fields))))
         (choice (org-anki-blocks-sync--ask-duplicate-resolution front-field)))
    (cond
     ((eq choice 'find-existing)
      (org-anki-blocks-sync--find-and-link-existing block deck fields))
     ((eq choice 'force-create)
      (org-anki-blocks-sync--force-create-note block deck model fields tags))
     ((eq choice 'delete-and-create)
      (org-anki-blocks-sync--delete-duplicates-and-create block deck model fields tags))
     ((eq choice 'skip)
      (message "Skipped creating duplicate note"))
     (t
      (error "Invalid duplicate resolution choice")))))

(defun org-anki-blocks-sync--ask-duplicate-resolution (front-content)
  "Ask user how to resolve duplicate with FRONT-CONTENT."
  (let ((buffer (get-buffer-create "*Org Anki Duplicate*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize "╔═══════════════════════════════════════════════════════════════════════════════╗\n" 'face 'bold))
      (insert (propertize "║                          DUPLICATE NOTE DETECTED                             ║\n" 'face 'bold))
      (insert (propertize "╚═══════════════════════════════════════════════════════════════════════════════╝\n" 'face 'bold))
      (insert "\n")
      (insert (propertize "A note with similar content already exists in Anki:\n\n" 'face 'warning))
      
      (insert (propertize "Content: " 'face 'bold))
      (insert (format "%s\n\n" (org-anki-blocks-sync--truncate-content front-content 200)))
      
      (insert (propertize "RESOLUTION OPTIONS:\n" 'face 'bold))
      (insert (propertize "  [f] Find and link to existing note\n" 'face 'success))
      (insert (propertize "  [c] Force create new note (modify content slightly)\n" 'face 'warning))
      (insert (propertize "  [d] Delete ALL remote duplicates and create new\n" 'face 'error))
      (insert (propertize "  [s] Skip this note\n" 'face 'shadow))
      (insert (propertize "  [q] Quit sync\n" 'face 'error))
      (insert "\n")
      (setq buffer-read-only t))
    
    (let ((window (display-buffer buffer '(display-buffer-pop-up-window . nil))))
      (select-window window)
      (fit-window-to-buffer window)
      
      (unwind-protect
          (let ((choice nil))
            (while (not choice)
              (let ((key (read-key "Choose duplicate resolution: ")))
                (cond
                 ((eq key ?f) (setq choice 'find-existing))
                 ((eq key ?c) (setq choice 'force-create))
                 ((eq key ?d) (setq choice 'delete-and-create))
                 ((eq key ?s) (setq choice 'skip))
                 ((eq key ?q) (error "Sync cancelled by user"))
                 (t (message "Invalid choice! Press [f], [c], [d], [s], or [q]")
                    (sit-for 1)))))
            choice)
        ;; Clean up window and buffer
        (when (window-live-p window)
          (delete-window window))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun org-anki-blocks-sync--truncate-content (content max-length)
  "Truncate CONTENT to MAX-LENGTH characters for display."
  (if (> (length content) max-length)
      (concat (substring content 0 (- max-length 3)) "...")
    content))

(defun org-anki-blocks-sync--find-and-link-existing (block deck fields)
  "Find existing note in DECK with similar FIELDS and link BLOCK to it."
  (let* ((search-term (or (cdr (assoc "Front" fields))
                         (cdr (assoc "Text" fields))
                         (cdr (car fields))))
         ;; Search for notes with similar content
         (query (format "deck:\"%s\" \"%s\"" deck (substring search-term 0 (min 50 (length search-term)))))
         (note-ids (org-anki-blocks-connect--find-notes query)))
    (if note-ids
        (let* ((notes-info (org-anki-blocks-connect--notes-info note-ids))
               (matching-note (org-anki-blocks-sync--find-best-match fields notes-info)))
          (if matching-note
              (let ((note-id (alist-get 'noteId matching-note)))
                (org-anki-blocks--update block 
                                       :id (format "%s" note-id)
                                       :hash (org-anki-blocks-sync--compute-hash fields))
                (message "Linked to existing note ID: %s" note-id))
            (message "Could not find exact match among %d similar notes" (length note-ids))))
      (message "No similar notes found in deck %s" deck))))

(defun org-anki-blocks-sync--find-best-match (fields notes-info)
  "Find the best matching note from NOTES-INFO for FIELDS."
  (let ((best-match nil)
        (best-score 0))
    (dolist (note-info notes-info)
      (let* ((remote-fields (org-anki-blocks-sync--extract-fields note-info))
             (score (org-anki-blocks-sync--compute-similarity fields remote-fields)))
        (when (> score best-score)
          (setq best-score score
                best-match note-info))))
    best-match))

(defun org-anki-blocks-sync--compute-similarity (fields1 fields2)
  "Compute similarity score between FIELDS1 and FIELDS2."
  (let ((total-fields (length fields1))
        (matching-fields 0))
    (dolist (field fields1)
      (let* ((field-name (car field))
             (field-value (cdr field))
             (remote-value (cdr (assoc field-name fields2))))
        (when (and remote-value 
                   (string= (string-trim field-value) (string-trim remote-value)))
          (setq matching-fields (1+ matching-fields)))))
    (if (> total-fields 0)
        (/ (float matching-fields) total-fields)
      0)))

(defun org-anki-blocks-sync--force-create-note (block deck model fields tags)
  "Force create a note for BLOCK by modifying FIELDS to avoid duplicates."
  (let* ((modified-fields (copy-sequence fields))
         (front-field (or (assoc "Front" modified-fields)
                         (assoc "Text" modified-fields)
                         (car modified-fields))))
    (when front-field
      ;; Add a small suffix to make it unique
      (setcdr front-field (concat (cdr front-field) " (variant)")))
    
    (condition-case err
        (let ((new-id (org-anki-blocks-connect--add-note deck model modified-fields tags)))
          (org-anki-blocks--update block
                                 :id (format "%s" new-id)
                                 :fields modified-fields
                                 :hash (org-anki-blocks-sync--compute-hash modified-fields))
          (message "Created note with modified content to avoid duplicate"))
      (error
       (error "Failed to create note even with modified content: %s" 
              (error-message-string err))))))

(defun org-anki-blocks-sync--delete-duplicates-and-create (block deck model fields tags)
  "Delete all duplicate notes and create a new one for BLOCK."
  (message "Finding all duplicate notes...")
  (let* ((search-term (or (cdr (assoc "Front" fields))
                         (cdr (assoc "Text" fields))
                         (cdr (car fields))))
         ;; Search for notes with similar content
         (query (format "deck:\"%s\" \"%s\"" deck (substring search-term 0 (min 50 (length search-term)))))
         (duplicate-ids (org-anki-blocks-connect--find-notes query)))
    
    (if duplicate-ids
        (progn
          (message "Found %d duplicate notes" (length duplicate-ids))
          ;; Confirm deletion
          (when (yes-or-no-p (format "Delete ALL %d duplicate notes and create new? " 
                                     (length duplicate-ids)))
            ;; Delete all duplicates
            (condition-case err
                (progn
                  (org-anki-blocks-connect--delete-notes duplicate-ids)
                  (message "Deleted %d duplicate notes" (length duplicate-ids))
                  
                  ;; Now create the new note
                  (let ((new-id (org-anki-blocks-connect--add-note deck model fields tags)))
                    (org-anki-blocks--update block
                                           :id (format "%s" new-id)
                                           :hash (org-anki-blocks-sync--compute-hash fields))
                    (message "Created new note with ID: %s" new-id)))
              (error
               (error "Failed to delete duplicates or create new note: %s"
                      (error-message-string err))))))
      (message "No duplicates found to delete"))))

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
  (catch 'quit-conflicts
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
          (message "Skipped conflict for note %s" (alist-get 'id conflict)))
         ((eq resolution 'quit)
          (message "Conflict resolution cancelled by user")
          (throw 'quit-conflicts nil)))))))

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