;;; yeetube-tests.el --- Tests for yeetube  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Run: emacs -Q --batch -L .. -l test/yeetube-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

;; Workaround: Emacs 31.1 (and Guix grafts of find-func.el) defines a
;; defcustom :set for `find-function-mode-lower-precedence' that
;; references `find-function-mode' before it exists.  Pre-declaring
;; it avoids a void-variable error when ert loads find-func indirectly.
(unless (boundp 'find-function-mode)
  (defvar find-function-mode nil))

(require 'ert)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'yeetube)

;;; ---- Group 1: yeetube-get-filter-code ----

(ert-deftest yeetube-test-get-filter-code-relevance ()
  "Relevance filter returns correct code."
  (should (string= "EgIQAQ%253D%253D" (yeetube-get-filter-code "Relevance"))))

(ert-deftest yeetube-test-get-filter-code-date ()
  "Date filter returns correct code."
  (should (string= "CAISAhAB" (yeetube-get-filter-code "Date"))))

(ert-deftest yeetube-test-get-filter-code-views ()
  "Views filter returns correct code."
  (should (string= "CAMSAhAB" (yeetube-get-filter-code "Views"))))

(ert-deftest yeetube-test-get-filter-code-rating ()
  "Rating filter returns correct code."
  (should (string= "CAESAhAB" (yeetube-get-filter-code "Rating"))))

(ert-deftest yeetube-test-get-filter-code-unknown ()
  "Unknown filter returns nil."
  (should-not (yeetube-get-filter-code "Nonexistent")))

;;; ---- Group 2: yeetube--callback error detection (regression) ----

(ert-deftest yeetube-test-callback-plist-get-arg-order ()
  "Verify plist-get extracts :error from a status plist correctly."
  ;; This is the core of the bug: (plist-get :error status) always returned nil.
  (let ((status '(:error (error http 404))))
    (should (equal '(error http 404) (plist-get status :error))))
  ;; No error case
  (let ((status '(:peer (:certificate ...))))
    (should-not (plist-get status :error))))

;;; ---- Group 3: yeetube--find-item ----

(ert-deftest yeetube-test-find-item ()
  "Find item plist by ID."
  (let ((yeetube-items '((:id "abc" :title "First" :type video)
                          (:id "def" :title "Second" :type video))))
    (should (equal "First" (plist-get (yeetube--find-item "abc") :title)))
    (should (equal "Second" (plist-get (yeetube--find-item "def") :title)))
    (should-not (yeetube--find-item "nonexistent"))))

;;; ---- Group 4: yeetube-get-url ----

(ert-deftest yeetube-test-get-url-video ()
  "Get URL for a video."
  (let ((yeetube-video-url "https://youtube.com/watch?v=")
        (yeetube-items '((:id "abc" :type video))))
    (should (equal "https://youtube.com/watch?v=abc"
                   (yeetube-get-url "abc" 'video)))))

(ert-deftest yeetube-test-get-url-playlist ()
  "Get URL for a playlist."
  (let ((yeetube-playlist-url "https://youtube.com/playlist?list="))
    (should (equal "https://youtube.com/playlist?list=PLxyz"
                   (yeetube-get-url "PLxyz" 'playlist)))))

;;; ---- Group 5: yeetube-mpv-play returns a process (regression) ----

(ert-deftest yeetube-test-mpv-play-returns-process ()
  "Verify yeetube-mpv-play returns the process object."
  (cl-letf (((symbol-function 'yeetube-mpv-check) #'ignore)
            ((symbol-function 'start-process-shell-command)
             (lambda (_name _buf _cmd)
               (start-process "yeetube-test-dummy" nil "true")))
            ((symbol-function 'get-process) (lambda (_n) nil)))
    (let* ((yeetube-mpv-program "mpv")
           (yeetube-mpv-enable-torsocks nil)
           (yeetube-mpv-video-quality "720")
           (yeetube-mpv-additional-flags nil)
           (proc (yeetube-mpv-play "https://example.com/video" "Test")))
      (should (processp proc))
      (when (process-live-p proc)
        (delete-process proc)))))

;;; ---- Group 6: yeetube-update-saved-videos-list round-trip (regression) ----

(ert-deftest yeetube-test-update-saved-videos-round-trip ()
  "Saved videos can be written and read back."
  ;; Remove the variable watcher to prevent triggers during let unwind
  (remove-variable-watcher 'yeetube-saved-videos #'yeetube-update-saved-videos-list)
  (unwind-protect
      (let* ((temp-dir (make-temp-file "yeetube-test" t))
             (user-emacs-directory (file-name-as-directory temp-dir))
             (yeetube-saved-videos nil)
             (test-data '(("Video One" . "https://youtube.com/watch?v=abc")
                          ("Video Two" . "https://youtube.com/watch?v=def"))))
        (unwind-protect
            (progn
              ;; Write via the watcher function
              (yeetube-update-saved-videos-list 'yeetube-saved-videos test-data nil nil)
              ;; Read back
              (setf yeetube-saved-videos nil)
              (yeetube-load-saved-videos)
              (should (equal test-data yeetube-saved-videos)))
          (delete-directory temp-dir t)))
    (add-variable-watcher 'yeetube-saved-videos #'yeetube-update-saved-videos-list)))

;;; ---- Group 7: yeetube-mode-map keybindings ----

(ert-deftest yeetube-test-keymap-S-not-bound ()
  "S key is not bound in yeetube-mode-map, leaving tabulated-list-sort accessible."
  (should-not (lookup-key yeetube-mode-map "S")))

(ert-deftest yeetube-test-keymap-L-bound-to-channel-streams ()
  "L key is bound to yeetube-channel-streams."
  (should (eq 'yeetube-channel-streams (lookup-key yeetube-mode-map "L"))))

(ert-deftest yeetube-test-keymap-s-bound-to-save-video ()
  "Lowercase s remains bound to yeetube-save-video."
  (should (eq 'yeetube-save-video (lookup-key yeetube-mode-map "s"))))

(ert-deftest yeetube-test-keymap-M-n-bound-to-next-page ()
  "M-n is bound to yeetube-next-page."
  (should (eq 'yeetube-next-page (lookup-key yeetube-mode-map (kbd "M-n")))))

;;; ---- Group 8: yeetube-mode buffer-local settings ----

(ert-deftest yeetube-test-mode-sets-truncate-string-ellipsis ()
  "yeetube-mode sets truncate-string-ellipsis to a single space."
  (let ((yeetube-content nil)
        (yeetube-display-thumbnails-p nil))
    (with-temp-buffer
      (yeetube-mode)
      (should (string= " " truncate-string-ellipsis)))))

(provide 'yeetube-tests)
;;; yeetube-tests.el ends here
