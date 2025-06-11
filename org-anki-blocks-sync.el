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

(defcustom org-anki-blocks-sync-duplicate-strategy 'ask
  "Strategy for handling duplicate notes during sync.
Possible values:
- `ask': Prompt user to resolve duplicates (default)
- `replace-with-remote': Update the local block with content from the remote duplicate
- `replace-with-local': Update remote with local content (or delete all if multiple)
- `skip': Skip creating duplicate notes"
  :type '(choice (const :tag "Ask user" ask)
                 (const :tag "Replace local with remote" replace-with-remote)
                 (const :tag "Replace remote with local" replace-with-local)
                 (const :tag "Skip duplicates" skip))
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
    
    (let ((window (display-buffer buffer 
                                   '((display-buffer-reuse-window
                                      display-buffer-pop-up-window)
                                     (reusable-frames . nil)
                                     (inhibit-same-window . t)))))
      (select-window window)
      ;; Make window take up most of the frame
      (let ((frame-height (frame-height))
            (frame-width (frame-width)))
        (set-window-text-height window (max 20 (- frame-height 10)))
        (when (> frame-width 100)
          (window-resize window (- 100 (window-width window)) t)))
      (goto-char (point-min))
      
      (unwind-protect
          (org-anki-blocks-sync--handle-conflict-input conflict buffer)
        (when (window-live-p window)
          (delete-window window))
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
  (insert (propertize "  [L] Use LOCAL for ALL remaining    " 'face 'success))
  (insert (propertize "[R] Use REMOTE for ALL remaining\n" 'face 'warning))
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
                              "Invalid choice! Press [l/L]ocal, [r/R]emote, [m]erge, [s]kip, or [q]uit: "
                            "Choose resolution: "))))
        (cond
         ((eq key ?l) (setq choice 'local))
         ((eq key ?r) (setq choice 'remote))
         ((eq key ?L) (setq choice 'local-all))
         ((eq key ?R) (setq choice 'remote-all))
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
        
        (let ((window (display-buffer buffer 
                                     '((display-buffer-reuse-window
                                        display-buffer-pop-up-window)
                                       (reusable-frames . nil)
                                       (inhibit-same-window . t)))))
          (select-window window)
          ;; Make window take up most of the frame
          (let ((frame-height (frame-height))
                (frame-width (frame-width)))
            (set-window-text-height window (max 20 (- frame-height 10)))
            (when (> frame-width 100)
              (window-resize window (- 100 (window-width window)) t)))
          
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
          (when (window-live-p window)
            (delete-window window))
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (error "Merge cancelled by user"))))
    
    (when (window-live-p window)
      (delete-window window))
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
             (let ((dup-result (org-anki-blocks-sync--handle-duplicate block deck model fields tags)))
               ;; Check if we need to update the duplicate strategy
               (when (cdr dup-result)
                 (setq org-anki-blocks-sync-duplicate-strategy (cdr dup-result))
                 (message "Duplicate strategy set to: %s for remaining duplicates" (cdr dup-result)))
               ;; Handle the result
               (if (car dup-result)
                   (progn
                     ;; Re-parse the block to get the updated properties
                     (goto-char (org-element-property :begin block))
                     (let* ((updated-block (org-element-at-point))
                            (new-id (org-anki-blocks--get-property updated-block :id)))
                       (if new-id
                           (progn
                             (message "Block synced successfully after duplicate resolution (ID: %s)" new-id)
                             t)
                         (progn
                           (message "Duplicate resolution succeeded but no ID found")
                           t))))
                 (progn
                   (message "Duplicate resolution skipped")
                   nil)))
           (signal (car err) (cdr err))))))
     ;; Update existing note
     (t
      (org-anki-blocks-connect--update-note id fields)
      (org-anki-blocks--update block
                             :hash (org-anki-blocks-sync--compute-hash fields))
      t))))

;;; Duplicate Handling

(defun org-anki-blocks-sync--handle-duplicate (block deck model fields tags)
  "Handle duplicate note creation for BLOCK with DECK, MODEL, FIELDS, and TAGS.
  Returns a list (result . strategy-change) where strategy-change can be nil or a new strategy."
  (let* ((front-field (or (cdr (assoc "Front" fields))
                         (cdr (assoc "Text" fields))
                         (cdr (car fields))))
         (choice (if (eq org-anki-blocks-sync-duplicate-strategy 'ask)
                     (org-anki-blocks-sync--ask-duplicate-resolution front-field)
                   org-anki-blocks-sync-duplicate-strategy))
         (result nil)
         (new-strategy nil))
    (cond
     ((memq choice '(replace-with-remote replace-with-remote-all))
      (setq result (org-anki-blocks-sync--replace-with-remote block deck fields))
      (when (eq choice 'replace-with-remote-all)
        (setq new-strategy 'replace-with-remote)))
     ((memq choice '(replace-with-local replace-with-local-all))
      (setq result (org-anki-blocks-sync--replace-with-local block deck model fields tags))
      (when (eq choice 'replace-with-local-all)
        (setq new-strategy 'replace-with-local)))
     ((memq choice '(skip skip-all))
      (message "Skipped creating duplicate note")
      (setq result nil)
      (when (eq choice 'skip-all)
        (setq new-strategy 'skip)))
     ((eq choice 'quit)
      (error "Sync cancelled by user"))
     (t
      (error "Invalid duplicate resolution choice")))
    (cons result new-strategy)))

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
      (insert (propertize "  [r] Replace local with remote content\n" 'face 'success))
      (insert (propertize "  [R] Replace local with remote for ALL duplicates\n" 'face 'success))
      (insert (propertize "  [l] Replace remote with local content (or delete all if multiple)\n" 'face 'warning))
      (insert (propertize "  [L] Replace remote with local for ALL duplicates\n" 'face 'warning))
      (insert (propertize "  [s] Skip this note\n" 'face 'shadow))
      (insert (propertize "  [S] Skip ALL duplicates\n" 'face 'shadow))
      (insert (propertize "  [q] Quit sync\n" 'face 'error))
      (insert "\n")
      (setq buffer-read-only t))
    
    (let ((window (display-buffer buffer 
                                   '((display-buffer-reuse-window
                                      display-buffer-pop-up-window)
                                     (reusable-frames . nil)
                                     (inhibit-same-window . t)))))
      (select-window window)
      ;; Make window a reasonable size for duplicate resolution
      (let ((frame-height (frame-height)))
        (set-window-text-height window (max 15 (min 30 (- frame-height 10)))))
      (goto-char (point-min))
      
      (unwind-protect
          (let ((choice nil))
            (while (not choice)
              (let ((key (read-key "Choose duplicate resolution: ")))
                (cond
                 ((eq key ?r) (setq choice 'replace-with-remote))
                 ((eq key ?R) (setq choice 'replace-with-remote-all))
                 ((eq key ?l) (setq choice 'replace-with-local))
                 ((eq key ?L) (setq choice 'replace-with-local-all))
                 ((eq key ?s) (setq choice 'skip))
                 ((eq key ?S) (setq choice 'skip-all))
                 ((eq key ?q) (setq choice 'quit))
                 (t (message "Invalid choice! Press [r/R], [l/L], [s/S], or [q]")
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

(defun org-anki-blocks-sync--replace-with-remote (block deck fields)
  "Replace local BLOCK content with remote duplicate content."
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
                ;; Update local block with remote content
                (org-anki-blocks-sync--pull-note matching-note)
                (message "Replaced local content with remote note ID: %s" note-id)
                t)  ; Return success
            (progn
              (message "Could not find exact match among %d similar notes" (length note-ids))
              nil)))  ; Return failure
      (progn
        (message "No similar notes found in deck %s" deck)
        nil))))  ; Return failure

(defun org-anki-blocks-sync--replace-with-local (block deck model fields tags)
  "Replace remote duplicate(s) with local BLOCK content.
If there's only one remote duplicate, update it.
If there are multiple duplicates, delete all and create new."
  (let* ((search-term (or (cdr (assoc "Front" fields))
                         (cdr (assoc "Text" fields))
                         (cdr (car fields))))
         ;; Search for notes with similar content
         (query (format "deck:\"%s\" \"%s\"" deck (substring search-term 0 (min 50 (length search-term)))))
         (duplicate-ids (org-anki-blocks-connect--find-notes query))
         (num-duplicates (length duplicate-ids)))
    (cond
     ;; No duplicates found
     ((= num-duplicates 0)
      (message "No duplicates found in deck %s" deck)
      nil)
     ;; Single duplicate - update it
     ((= num-duplicates 1)
      (let ((note-id (car duplicate-ids)))
        (message "Updating single duplicate note ID: %s" note-id)
        ;; Update the existing note with local content
        (org-anki-blocks-connect--update-note note-id fields)
        ;; Link the local block to this note
        (org-anki-blocks--update block
                               :id (format "%s" note-id)
                               :hash (org-anki-blocks-sync--compute-hash fields))
        (message "Updated remote duplicate with local content")
        t))  ; Return success
     ;; Multiple duplicates - delete all and create new
     (t
      (org-anki-blocks-sync--delete-duplicates-and-create block deck model fields tags)))))

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

(defun org-anki-blocks-sync--delete-duplicates-and-create (block deck model fields tags)
  "Delete all duplicate notes and create a new one for BLOCK.
Returns t if successful, nil if no action taken."
  (message "Finding all duplicate notes...")
  (let* ((search-term (or (cdr (assoc "Front" fields))
                         (cdr (assoc "Text" fields))
                         (cdr (car fields))))
         ;; Search for notes with similar content
         (query (format "deck:\"%s\" \"%s\"" deck (substring search-term 0 (min 50 (length search-term)))))
         (duplicate-ids (org-anki-blocks-connect--find-notes query))
         ;; Check if we're in batch mode
         (batch-mode (not (eq org-anki-blocks-sync-duplicate-strategy 'ask))))
    
    (if duplicate-ids
        (progn
          (message "Found %d duplicate notes" (length duplicate-ids))
          ;; Confirm deletion only in interactive mode
          (if (or batch-mode
                  (yes-or-no-p (format "Delete ALL %d duplicate notes and create new? " 
                                       (length duplicate-ids))))
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
                      (message "Created new note with ID: %s" new-id))
                    t)  ; Return success
                (error
                 (error "Failed to delete duplicates or create new note: %s"
                        (error-message-string err))))
            (progn
              (message "Cancelled deleting duplicates")
              nil)))  ; Return nil if cancelled
      (progn
        (message "No duplicates found to delete")
        nil))))  ; Return nil if no duplicates

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
  (let ((pushed 0)
        (errors 0)
        (skipped 0)
        (cancelled nil)
        (original-duplicate-strategy org-anki-blocks-sync-duplicate-strategy))
    (unwind-protect
        (catch 'sync-cancelled
          ;; Process blocks from end to beginning to avoid position shifts
          (let ((blocks (nreverse (org-anki-blocks--find-all))))
            (dolist (block blocks)
          (condition-case err
              (when (or (not (org-anki-blocks--get-property block :id))
                        (org-anki-blocks--get-property block :modified)
                        (org-anki-blocks--get-property block :deleted))
                (let ((result (org-anki-blocks-sync--push-note block)))
                  (cond
                   ((eq result t) (cl-incf pushed))
                   ((eq result nil) (cl-incf skipped))
                   (t (cl-incf pushed)))))  ; For backwards compatibility
            (error
             ;; Check if user cancelled sync
             (if (string-match-p "cancelled by user" (error-message-string err))
                 (progn
                   (setq cancelled t)
                   (throw 'sync-cancelled nil))
               (cl-incf errors)
               (message "Error pushing note: %s" (error-message-string err))))))))
      ;; Restore original duplicate strategy
      (setq org-anki-blocks-sync-duplicate-strategy original-duplicate-strategy))
    (if cancelled
        (message "Sync cancelled by user after %d notes pushed, %d skipped" pushed skipped)
      (message "Push complete: %d notes pushed, %d skipped, %d errors" pushed skipped errors))))

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
  (let ((remaining-strategy nil)
        (conflicts-processed 0)
        (total-conflicts (length conflicts)))
    (catch 'quit-conflicts
      (dolist (conflict conflicts)
        (cl-incf conflicts-processed)
        (let ((resolution (if remaining-strategy
                             remaining-strategy
                           (let ((res (org-anki-blocks-sync--resolve-conflict conflict)))
                             ;; Check if user chose to apply to all
                             (cond
                              ((eq res 'local-all)
                               (setq remaining-strategy 'local)
                               (message "Using LOCAL for all remaining %d conflicts" 
                                       (- total-conflicts conflicts-processed))
                               'local)
                              ((eq res 'remote-all)
                               (setq remaining-strategy 'remote)
                               (message "Using REMOTE for all remaining %d conflicts" 
                                       (- total-conflicts conflicts-processed))
                               'remote)
                              (t res))))))
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
          (throw 'quit-conflicts nil))))))))

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

;;; Utility Commands

(defun org-anki-blocks-sync-set-duplicate-strategy (strategy)
  "Set the duplicate handling STRATEGY for the current session.
Valid strategies: ask, replace-with-remote, replace-with-local, skip."
  (interactive
   (list (intern (completing-read "Duplicate strategy: "
                                  '("ask" "replace-with-remote" "replace-with-local" "skip")
                                  nil t))))
  (setq org-anki-blocks-sync-duplicate-strategy strategy)
  (message "Duplicate strategy set to: %s" strategy))

(provide 'org-anki-blocks-sync)
;;; org-anki-blocks-sync.el ends here