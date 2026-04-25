;;; keymap-popup.el --- Described keymaps with popup help  -*- lexical-binding: t; -*-

;; Author: Thanos Apollo
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience keymaps
;; URL: https://thanosapollo.org/projects/keymap-popup

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A single macro `define-described-keymap' that produces both a real
;; `defvar-keymap' (for direct key dispatch) and stored descriptions
;; (for a popup help window).  One definition, two uses.

;; Vendored dependency from <https://codeberg.org/thanosapollo/emacs-keymap-popup>

;;; Code:

(require 'cl-lib)

(defgroup keymap-popup nil
  "Described keymaps with popup help."
  :group 'convenience)

;;; Faces

(defface keymap-popup-key
  '((t :inherit help-key-binding))
  "Face for key bindings in the popup."
  :group 'keymap-popup)

(defface keymap-popup-group-header
  '((t :inherit bold))
  "Face for group headers in the popup."
  :group 'keymap-popup)

(defface keymap-popup-value
  '((t :inherit font-lock-string-face :weight bold))
  "Face for infix values in the popup."
  :group 'keymap-popup)

;;; Parsers

(defun keymap-popup--extract-props (plist)
  "Extract known properties from PLIST.
Recognized keys: :if, :reader, :prompt, :stay-open."
  (cl-loop for (k v) on plist by #'cddr
           when (memq k '(:if :reader :prompt :stay-open))
           append (list k v)))

(defun keymap-popup--parse-entry (key spec)
  "Parse binding SPEC for KEY into a plist.
SPEC is (DESCRIPTION COMMAND-OR-TYPE &rest PROPS)."
  (let* ((description (car spec))
         (second (cadr spec))
         (rest (cddr spec)))
    (pcase second
      (:switch
       `(:key ,key :description ,description :type switch
              :variable ,(car rest)
              ,@(keymap-popup--extract-props (cdr rest))))
      (:option
       `(:key ,key :description ,description :type option
              :variable ,(car rest)
              ,@(keymap-popup--extract-props (cdr rest))))
      (_
       `(:key ,key :description ,description :type suffix
              :command ,second
              ,@(keymap-popup--extract-props rest))))))

(defun keymap-popup--split-groups (bindings)
  "Split BINDINGS at :group and :row keywords.
Returns a list of rows, each row a list of (NAME . FLAT-ENTRIES) chunks.
`:group' starts a new group within the current row.
`:row' starts a new row."
  (keymap-popup--split-groups-1 bindings nil nil nil nil))

(defun keymap-popup--split-groups-1 (rest name entries groups rows)
  "Recursive helper for `keymap-popup--split-groups'.
REST is remaining bindings, NAME is current group name, ENTRIES
is accumulated entries (reversed), GROUPS is current row's groups
\(reversed), ROWS is accumulated rows (reversed)."
  (let ((flush-group (if entries
                         (cons (cons name (reverse entries)) groups)
                       groups)))
    (cond
     ((null rest)
      (reverse (if flush-group
                    (cons (reverse flush-group) rows)
                  rows)))
     ((eq (car rest) :row)
      (keymap-popup--split-groups-1
       (cdr rest) nil nil nil
       (if flush-group (cons (reverse flush-group) rows) rows)))
     ((eq (car rest) :group)
      (keymap-popup--split-groups-1
       (cddr rest) (cadr rest) nil flush-group rows))
     (t
      (keymap-popup--split-groups-1
       (cddr rest) name
       (cons (cons (car rest) (cadr rest)) entries)
       groups rows)))))

(defun keymap-popup--parse-chunk (chunk)
  "Parse CHUNK of (NAME . ((KEY . SPEC) ...)) into a group plist."
  (let* ((name (car chunk))
         (pairs (cdr chunk))
         (entries (mapcar (lambda (pair)
                            (keymap-popup--parse-entry (car pair) (cdr pair)))
                          pairs)))
    (list :name name :entries entries)))

(defun keymap-popup--parse-bindings (bindings)
  "Parse BINDINGS into a list of rows.
Each row is a list of group plists with :name and :entries."
  (mapcar (lambda (row) (mapcar #'keymap-popup--parse-chunk row))
          (keymap-popup--split-groups bindings)))

;;; Infix generators

(defun keymap-popup--switch-forms (map-name entry)
  "Return (defvar-local defun) forms for switch ENTRY in MAP-NAME."
  (let* ((variable (plist-get entry :variable))
         (description (plist-get entry :description))
         (fn-name (intern (format "%s--toggle-%s" map-name variable))))
    (list
     `(defvar-local ,variable nil)
     `(defun ,fn-name ()
        ,(format "Toggle %s." description)
        (interactive)
        (setq-local ,variable (not ,variable))
        (message "%s: %s" ,description (if ,variable "on" "off"))))))

(defun keymap-popup--option-forms (map-name entry)
  "Return (defvar-local defun) forms for option ENTRY in MAP-NAME."
  (let* ((variable (plist-get entry :variable))
         (description (plist-get entry :description))
         (reader (or (plist-get entry :reader) 'read-string))
         (prompt (or (plist-get entry :prompt) (format "%s: " description)))
         (fn-name (intern (format "%s--set-%s" map-name variable))))
    (list
     `(defvar-local ,variable nil)
     `(defun ,fn-name ()
        ,(format "Set %s." description)
        (interactive)
        (setq-local ,variable (,reader ,prompt))
        (message "%s: %s" ,description ,variable)))))

(defun keymap-popup--entry-command (map-name entry)
  "Return the command to bind in MAP-NAME's keymap for ENTRY."
  (pcase (plist-get entry :type)
    ('suffix (plist-get entry :command))
    ('switch (intern (format "%s--toggle-%s" map-name (plist-get entry :variable))))
    ('option (intern (format "%s--set-%s" map-name (plist-get entry :variable))))))

;;; Macro helpers

(defun keymap-popup--build-keymap-pairs (map-name entries)
  "Build flat key/command list for `defvar-keymap' from ENTRIES.
MAP-NAME is used to derive generated command names."
  (cl-loop for entry in entries
           for cmd = (keymap-popup--entry-command map-name entry)
           append (list (plist-get entry :key)
                        (if (symbolp cmd) `#',cmd cmd))))

(defun keymap-popup--build-entry-form (entry)
  "Build a `list' form for a single ENTRY that evaluates lambdas properly."
  (let* ((desc (plist-get entry :description))
         (desc-form (if (and (consp desc) (eq (car desc) 'lambda))
                        desc
                      `',desc))
         (type (plist-get entry :type)))
    (pcase type
      ('suffix
       (let* ((cmd (plist-get entry :command))
              (cmd-form (if (and (consp cmd) (eq (car cmd) 'lambda))
                            cmd
                          `',cmd))
              (if-pred (plist-get entry :if))
              (stay-open (plist-get entry :stay-open)))
         `(list :key ,(plist-get entry :key)
                :description ,desc-form
                :type 'suffix
                :command ,cmd-form
                ,@(when if-pred (list :if if-pred))
                ,@(when stay-open (list :stay-open t)))))
      ('switch
       (let ((if-pred (plist-get entry :if)))
         `(list :key ,(plist-get entry :key)
                :description ,desc-form
                :type 'switch
                :variable ',(plist-get entry :variable)
                ,@(when if-pred (list :if if-pred)))))
      ('option
       (let ((if-pred (plist-get entry :if)))
         `(list :key ,(plist-get entry :key)
                :description ,desc-form
                :type 'option
                :variable ',(plist-get entry :variable)
                ,@(when if-pred (list :if if-pred))))))))

(defun keymap-popup--build-descriptions-form (rows)
  "Build a `list' form that constructs descriptions at load time.
ROWS is a list of rows, each row a list of groups.
Uses list calls so lambdas get compiled."
  `(list ,@(mapcar
            (lambda (row)
              `(list ,@(mapcar
                        (lambda (group)
                          `(list :name ,(plist-get group :name)
                                 :entries (list ,@(mapcar #'keymap-popup--build-entry-form
                                                         (plist-get group :entries)))))
                        row)))
            rows)))

;;; Macro

(defun keymap-popup--extract-macro-opts (body)
  "Extract macro options from BODY.
Returns (DOCSTRING POPUP-KEY BINDINGS) where DOCSTRING and
POPUP-KEY may be nil.  A string followed by a list is a key
binding, not a docstring."
  (let* ((docstring (when (and (stringp (car body))
                               (not (listp (cadr body))))
                      (car body)))
         (rest (if docstring (cdr body) body))
         (has-popup-key (eq (car rest) :popup-key))
         (popup-key (if has-popup-key (cadr rest) "h"))
         (bindings (if has-popup-key (cddr rest) rest)))
    (list docstring popup-key bindings)))

;;;###autoload
(defmacro define-described-keymap (name &rest body)
  "Define NAME as a keymap with embedded descriptions.
BODY is an optional docstring, optional :popup-key KEY (default
\"h\"), followed by :group keywords and KEY (DESC ...) pairs."
  (declare (indent 1))
  (let* ((opts (keymap-popup--extract-macro-opts body))
         (docstring (nth 0 opts))
         (popup-key (nth 1 opts))
         (bindings (nth 2 opts))
         (rows (keymap-popup--parse-bindings bindings))
         (all-entries (cl-loop for row in rows
                               append (cl-loop for group in row
                                               append (plist-get group :entries))))
         (infix-forms (cl-loop for entry in all-entries
                               append (pcase (plist-get entry :type)
                                        ('switch (keymap-popup--switch-forms name entry))
                                        ('option (keymap-popup--option-forms name entry))
                                        (_ nil))))
         (keymap-pairs (keymap-popup--build-keymap-pairs name all-entries)))
    `(progn
       ,@infix-forms
       (defvar-keymap ,name
         ,@(when docstring (list :doc docstring))
         ,@keymap-pairs
         ,popup-key (lambda () (interactive) (keymap-popup ',name)))
       (put ',name 'keymap-popup--descriptions
            ,(keymap-popup--build-descriptions-form rows)))))

;;; Public API

(defun keymap-popup--group-exists-p (rows group-name)
  "Return non-nil if GROUP-NAME exists in ROWS."
  (cl-loop for row in rows
           thereis (cl-loop for group in row
                            thereis (equal (plist-get group :name) group-name))))

(defun keymap-popup--append-to-group (rows entry group-name)
  "Return ROWS with ENTRY appended to groups matching GROUP-NAME."
  (mapcar (lambda (row)
            (mapcar (lambda (group)
                      (if (equal (plist-get group :name) group-name)
                          (list :name (plist-get group :name)
                                :entries (append (plist-get group :entries)
                                                 (list entry)))
                        group))
                    row))
          rows))

(defun keymap-popup--add-entry-to-rows (rows entry group-name)
  "Return ROWS with ENTRY appended to the group named GROUP-NAME.
Falls back to the first group if GROUP-NAME is not found."
  (keymap-popup--append-to-group
   rows entry
   (if (keymap-popup--group-exists-p rows group-name)
       group-name
     (plist-get (caar rows) :name))))

(defun keymap-popup--remove-key-from-rows (rows key)
  "Return ROWS with entries matching KEY filtered out."
  (mapcar (lambda (row)
            (mapcar (lambda (group)
                      (list :name (plist-get group :name)
                            :entries (cl-remove-if
                                      (lambda (e) (equal (plist-get e :key) key))
                                      (plist-get group :entries))))
                    row))
          rows))

;;;###autoload
(defun keymap-popup-add-entry (map-symbol key description command &optional group)
  "Add KEY binding with DESCRIPTION and COMMAND to MAP-SYMBOL.
GROUP is the group name to add to (nil for the first group).
Updates both the keymap and the popup descriptions."
  (or (get map-symbol 'keymap-popup--descriptions)
      (user-error "No descriptions for `%s'" map-symbol))
  (keymap-set (symbol-value map-symbol) key command)
  (let ((entry (list :key key :description description
                     :type 'suffix :command command)))
    (put map-symbol 'keymap-popup--descriptions
         (keymap-popup--add-entry-to-rows
          (get map-symbol 'keymap-popup--descriptions) entry group))))

;;;###autoload
(defun keymap-popup-remove-entry (map-symbol key)
  "Remove KEY binding from MAP-SYMBOL.
Updates both the keymap and the popup descriptions."
  (keymap-set (symbol-value map-symbol) key nil)
  (put map-symbol 'keymap-popup--descriptions
       (keymap-popup--remove-key-from-rows
        (get map-symbol 'keymap-popup--descriptions) key)))

;;; Renderer

(defun keymap-popup--resolve-description (desc)
  "If DESC is a function, call it; otherwise return as-is."
  (if (functionp desc) (funcall desc) desc))

(defun keymap-popup--render-entry (entry)
  "Render ENTRY into a formatted line, or nil if :if hides it."
  (when (or (null (plist-get entry :if))
            (funcall (plist-get entry :if)))
    (let* ((desc (keymap-popup--resolve-description
                  (plist-get entry :description)))
           (key-str (propertize (plist-get entry :key)
                                'face 'keymap-popup-key))
           (value-str (pcase (plist-get entry :type)
                        ('switch (propertize
                                  (if (symbol-value (plist-get entry :variable))
                                      " [on]" " [off]")
                                  'face 'keymap-popup-value))
                        ('option (propertize
                                  (format " =%s"
                                          (symbol-value (plist-get entry :variable)))
                                  'face 'keymap-popup-value))
                        (_ ""))))
      (format "  %s  %s%s" key-str desc value-str))))

(defun keymap-popup--render-group-lines (group)
  "Render GROUP into a list of lines (strings).
Returns nil if the group has no visible entries."
  (let* ((header (when-let* ((raw-name (plist-get group :name))
                             (name (keymap-popup--resolve-description raw-name)))
                   (propertize name 'face 'keymap-popup-group-header)))
         (lines (cl-loop for entry in (plist-get group :entries)
                         for line = (keymap-popup--render-entry entry)
                         when line collect line)))
    (when lines
      (if header (cons header lines) lines))))

(defun keymap-popup--string-width-visible (str)
  "Return the visible width of STR, ignoring text properties."
  (string-width (substring-no-properties str)))

(defun keymap-popup--pad-line (line width)
  "Pad LINE with spaces to WIDTH (based on visible characters)."
  (let ((visible-width (keymap-popup--string-width-visible line)))
    (if (< visible-width width)
        (concat line (make-string (- width visible-width) ?\s))
      line)))

(defun keymap-popup--column-width (col)
  "Return the max visible width of lines in COL."
  (cl-loop for line in col
           maximize (keymap-popup--string-width-visible line)))

(defun keymap-popup--join-columns (columns separator col-widths)
  "Join COLUMNS side by side with SEPARATOR between them.
COL-WIDTHS is a list of minimum widths per column position.
Shorter columns are padded with blank lines."
  (let* ((max-height (cl-loop for col in columns maximize (length col)))
         (padded-cols (cl-mapcar
                       (lambda (col width)
                         (let ((padded (mapcar (lambda (line)
                                                (keymap-popup--pad-line line width))
                                              col))
                               (blanks (make-list (- max-height (length col))
                                                  (make-string width ?\s))))
                           (append padded blanks)))
                       columns col-widths)))
    (cl-loop for row from 0 below max-height
             collect (string-trim-right
                      (mapconcat (lambda (col) (nth row col))
                                 padded-cols
                                 separator)))))

(defun keymap-popup--rows-to-columns (rows)
  "Render each row of ROWS into its list of column line-lists.
Returns a list of ((col-lines ...) ...) per row, filtering empty groups."
  (mapcar (lambda (row)
            (cl-loop for group in row
                     for lines = (keymap-popup--render-group-lines group)
                     when lines collect lines))
          rows))

(defun keymap-popup--global-col-widths (rendered-rows)
  "Compute max column width per position across all RENDERED-ROWS."
  (let ((max-cols (cl-loop for cols in rendered-rows
                           maximize (length cols))))
    (cl-loop for i from 0 below max-cols
             collect (cl-loop for cols in rendered-rows
                              when (nth i cols)
                              maximize (keymap-popup--column-width (nth i cols))))))

(defun keymap-popup--render (docstring rows)
  "Render DOCSTRING and ROWS into a complete popup string.
ROWS is a list of rows, each row a list of groups.
Column widths are aligned across all rows."
  (let* ((doc (when docstring
                (concat (propertize docstring 'face 'font-lock-doc-face)
                        "\n")))
         (rendered-rows (keymap-popup--rows-to-columns rows))
         (col-widths (keymap-popup--global-col-widths rendered-rows))
         (sections (cl-loop for cols in rendered-rows
                            when cols
                            collect (mapconcat #'identity
                                              (keymap-popup--join-columns
                                               cols "   " col-widths)
                                              "\n"))))
    (concat doc (mapconcat #'identity sections "\n") "\n")))

;;; Popup display

(defun keymap-popup--find-entry-by-key (descriptions key-str)
  "Find the entry matching KEY-STR in DESCRIPTIONS.
DESCRIPTIONS is a list of rows, each row a list of groups.
Returns the entry plist, or nil."
  (cl-loop for row in descriptions
           thereis (cl-loop for group in row
                            thereis (cl-loop for entry in (plist-get group :entries)
                                             when (equal (plist-get entry :key) key-str)
                                             return entry))))

(defun keymap-popup--infix-p (descriptions key-str)
  "Return non-nil if KEY-STR maps to an infix entry in DESCRIPTIONS."
  (when-let* ((entry (keymap-popup--find-entry-by-key descriptions key-str)))
    (memq (plist-get entry :type) '(switch option))))

(defun keymap-popup--stay-open-p (descriptions key-str)
  "Return non-nil if KEY-STR should keep the popup open.
True for infixes and suffixes with :stay-open."
  (when-let* ((entry (keymap-popup--find-entry-by-key descriptions key-str)))
    (or (memq (plist-get entry :type) '(switch option))
        (plist-get entry :stay-open))))

(defun keymap-popup--refresh-buffer (buf map-symbol)
  "Re-render the popup buffer BUF for MAP-SYMBOL."
  (let* ((descriptions (get map-symbol 'keymap-popup--descriptions))
         (docstring (documentation-property map-symbol 'variable-documentation))
         (content (keymap-popup--render docstring descriptions)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))))))

(defun keymap-popup--prepare-buffer (map-symbol)
  "Create and populate the popup buffer for MAP-SYMBOL."
  (let ((buf (get-buffer-create "*keymap-popup*")))
    (or (get map-symbol 'keymap-popup--descriptions)
        (user-error "No descriptions for `%s'" map-symbol))
    (with-current-buffer buf
      (setq-local buffer-read-only t)
      (setq-local cursor-type nil)
      (setq-local mode-line-format nil))
    (keymap-popup--refresh-buffer buf map-symbol)
    buf))

(defun keymap-popup--dismiss-p (key _cmd)
  "Return non-nil if KEY should dismiss the popup.
The keys `q' and `C-g' always dismiss."
  (memq key '(?q ?\C-g)))

(defun keymap-popup--read-loop (buf win map-symbol keymap descriptions)
  "Read keys until a suffix or dismiss key is pressed.
MAP-SYMBOL identifies the described keymap.  KEYMAP is the live
keymap for command lookup.  DESCRIPTIONS is the stored metadata.
Stay-open keys (infixes and :stay-open suffixes) execute and
re-render BUF in WIN.  Returns the command to dispatch, or nil."
  (cl-loop for key = (read-key)
           for key-str = (key-description (vector key))
           for cmd = (keymap-lookup keymap key-str)
           when (keymap-popup--dismiss-p key cmd) return nil
           when (and cmd (keymap-popup--stay-open-p descriptions key-str))
           do (progn
                (call-interactively cmd)
                (keymap-popup--refresh-buffer buf map-symbol)
                (when (and win (window-live-p win))
                  (fit-window-to-buffer win)))
           when (and cmd (not (keymap-popup--stay-open-p descriptions key-str)))
           return cmd))

;;;###autoload
(defun keymap-popup (map-symbol)
  "Show popup help for described keymap MAP-SYMBOL.
Display in a bottom side window.  Infix keys (switches/options)
execute and re-render without closing.  Suffix keys and dismiss
keys close the popup."
  (interactive)
  (let* ((buf (keymap-popup--prepare-buffer map-symbol))
         (keymap (symbol-value map-symbol))
         (descriptions (get map-symbol 'keymap-popup--descriptions)))
    (unwind-protect
        (let* ((win (display-buffer buf
                                    '(display-buffer-in-side-window
                                      (side . bottom)
                                      (window-height . fit-window-to-buffer))))
               (_ (when win (fit-window-to-buffer win)))
               (final-cmd (keymap-popup--read-loop
                           buf win map-symbol keymap descriptions)))
          (when (and win (window-live-p win))
            (delete-window win))
          (when final-cmd
            (call-interactively final-cmd)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(provide 'keymap-popup)
;;; keymap-popup.el ends here
