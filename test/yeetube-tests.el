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

(load (expand-file-name "../yeetube.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; ---- Group 1: yeetube-view-count-format ----

(ert-deftest yeetube-test-view-count-format-empty ()
  "Empty string returns empty."
  (should (string= "" (yeetube-view-count-format ""))))

(ert-deftest yeetube-test-view-count-format-single-digit ()
  "Single digit has no commas."
  (should (string= "5" (yeetube-view-count-format "5"))))

(ert-deftest yeetube-test-view-count-format-hundreds ()
  "Hundreds have no commas."
  (should (string= "999" (yeetube-view-count-format "999"))))

(ert-deftest yeetube-test-view-count-format-thousands ()
  "Thousands get one comma."
  (should (string= "1,000" (yeetube-view-count-format "1000"))))

(ert-deftest yeetube-test-view-count-format-millions ()
  "Millions get two commas."
  (should (string= "1,234,567" (yeetube-view-count-format "1234567"))))

(ert-deftest yeetube-test-view-count-format-with-text ()
  "Non-digit characters are stripped before formatting."
  (should (string= "1,234" (yeetube-view-count-format "1,234 views"))))

;;; ---- Group 2: yeetube-get-filter-code ----

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

;;; ---- Group 3: yeetube--duration-to-seconds ----

(ert-deftest yeetube-test-duration-to-seconds-hhmmss ()
  "HH:MM:SS format converts correctly."
  (should (= 3661 (yeetube--duration-to-seconds "1:01:01"))))

(ert-deftest yeetube-test-duration-to-seconds-mmss ()
  "MM:SS format converts correctly."
  (should (= 125 (yeetube--duration-to-seconds "2:05"))))

(ert-deftest yeetube-test-duration-to-seconds-ss ()
  "SS-only format converts correctly."
  (should (= 45 (yeetube--duration-to-seconds "45"))))

(ert-deftest yeetube-test-duration-to-seconds-zero ()
  "Zero duration."
  (should (= 0 (yeetube--duration-to-seconds "0:00"))))

(ert-deftest yeetube-test-duration-to-seconds-large ()
  "Large duration converts correctly."
  (should (= 36610 (yeetube--duration-to-seconds "10:10:10"))))

;;; ---- Group 4: yeetube--parse-relative-date ----

(ert-deftest yeetube-test-parse-relative-date-seconds ()
  "Seconds parsed correctly."
  (should (= 30 (yeetube--parse-relative-date "30 seconds ago"))))

(ert-deftest yeetube-test-parse-relative-date-minutes ()
  "Minutes parsed correctly."
  (should (= 300 (yeetube--parse-relative-date "5 minutes ago"))))

(ert-deftest yeetube-test-parse-relative-date-hours ()
  "Hours parsed correctly."
  (should (= 7200 (yeetube--parse-relative-date "2 hours ago"))))

(ert-deftest yeetube-test-parse-relative-date-days ()
  "Days parsed correctly."
  (should (= 259200 (yeetube--parse-relative-date "3 days ago"))))

(ert-deftest yeetube-test-parse-relative-date-weeks ()
  "Weeks parsed correctly."
  (should (= 604800 (yeetube--parse-relative-date "1 week ago"))))

(ert-deftest yeetube-test-parse-relative-date-months ()
  "Months parsed correctly (30-day month)."
  (should (= 5184000 (yeetube--parse-relative-date "2 months ago"))))

(ert-deftest yeetube-test-parse-relative-date-years ()
  "Years parsed correctly (365-day year)."
  (should (= 31536000 (yeetube--parse-relative-date "1 year ago"))))

(ert-deftest yeetube-test-parse-relative-date-unknown-unit ()
  "Unknown unit returns 0."
  (should (= 0 (yeetube--parse-relative-date "5 fortnights ago"))))

;;; ---- Group 5: yeetube--callback error detection (regression) ----

(ert-deftest yeetube-test-callback-plist-get-arg-order ()
  "Verify plist-get extracts :error from a status plist correctly."
  ;; This is the core of the bug: (plist-get :error status) always returned nil.
  (let ((status '(:error (error http 404))))
    (should (equal '(error http 404) (plist-get status :error))))
  ;; No error case
  (let ((status '(:peer (:certificate ...))))
    (should-not (plist-get status :error))))

;;; ---- Group 6: Sort functions with and without thumbnails (regression) ----

(ert-deftest yeetube-test-sort-views-with-thumbnails ()
  "Sort by views works when thumbnails are enabled (index 2)."
  (let ((yeetube-display-thumbnails-p t)
        (a '("id1" ["thumb" "Title A" "1,000" "3:00" "1 day ago" "Ch"]))
        (b '("id2" ["thumb" "Title B" "2,000" "5:00" "2 days ago" "Ch"])))
    (should (yeetube--sort-views a b))
    (should-not (yeetube--sort-views b a))))

(ert-deftest yeetube-test-sort-views-without-thumbnails ()
  "Sort by views works when thumbnails are disabled (index 1)."
  (let ((yeetube-display-thumbnails-p nil)
        (a '("id1" ["Title A" "1,000" "3:00" "1 day ago" "Ch"]))
        (b '("id2" ["Title B" "2,000" "5:00" "2 days ago" "Ch"])))
    (should (yeetube--sort-views a b))
    (should-not (yeetube--sort-views b a))))

(ert-deftest yeetube-test-sort-duration-with-thumbnails ()
  "Sort by duration works when thumbnails are enabled (index 3)."
  (let ((yeetube-display-thumbnails-p t)
        (a '("id1" ["thumb" "Title A" "100" "1:00" "1 day ago" "Ch"]))
        (b '("id2" ["thumb" "Title B" "200" "2:00" "2 days ago" "Ch"])))
    (should (yeetube--sort-duration a b))
    (should-not (yeetube--sort-duration b a))))

(ert-deftest yeetube-test-sort-duration-without-thumbnails ()
  "Sort by duration works when thumbnails are disabled (index 2)."
  (let ((yeetube-display-thumbnails-p nil)
        (a '("id1" ["Title A" "100" "1:00" "1 day ago" "Ch"]))
        (b '("id2" ["Title B" "200" "2:00" "2 days ago" "Ch"])))
    (should (yeetube--sort-duration a b))
    (should-not (yeetube--sort-duration b a))))

(ert-deftest yeetube-test-sort-date-with-thumbnails ()
  "Sort by date works when thumbnails are enabled (index 4)."
  (let ((yeetube-display-thumbnails-p t)
        (a '("id1" ["thumb" "Title A" "100" "1:00" "1 day ago" "Ch"]))
        (b '("id2" ["thumb" "Title B" "200" "2:00" "2 days ago" "Ch"])))
    (should (yeetube--sort-date a b))
    (should-not (yeetube--sort-date b a))))

(ert-deftest yeetube-test-sort-date-without-thumbnails ()
  "Sort by date works when thumbnails are disabled (index 3)."
  (let ((yeetube-display-thumbnails-p nil)
        (a '("id1" ["Title A" "100" "1:00" "1 day ago" "Ch"]))
        (b '("id2" ["Title B" "200" "2:00" "2 days ago" "Ch"])))
    (should (yeetube--sort-date a b))
    (should-not (yeetube--sort-date b a))))

;;; ---- Group 7: yeetube-mpv-play returns a process (regression) ----

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

;;; ---- Group 8: yeetube-update-saved-videos-list round-trip (regression) ----

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

;;; ---- Group 9: yeetube-mode-map keybindings ----

(ert-deftest yeetube-test-keymap-S-not-bound ()
  "S key is not bound in yeetube-mode-map, leaving tabulated-list-sort accessible."
  (should-not (lookup-key yeetube-mode-map "S")))

(ert-deftest yeetube-test-keymap-L-bound-to-channel-streams ()
  "L key is bound to yeetube-channel-streams."
  (should (eq 'yeetube-channel-streams (lookup-key yeetube-mode-map "L"))))

(ert-deftest yeetube-test-keymap-s-bound-to-save-video ()
  "Lowercase s remains bound to yeetube-save-video."
  (should (eq 'yeetube-save-video (lookup-key yeetube-mode-map "s"))))

;;; ---- Group 10: yeetube-mode buffer-local settings ----

(ert-deftest yeetube-test-mode-sets-truncate-string-ellipsis ()
  "yeetube-mode sets truncate-string-ellipsis to a single space."
  (let ((yeetube-content nil)
        (yeetube-display-thumbnails-p nil))
    (with-temp-buffer
      (yeetube-mode)
      (should (string= " " truncate-string-ellipsis)))))

;;; ---- Group 11: Thumbnail image persistence on entry vector ----

(ert-deftest yeetube-test-image-callback-persists-image-on-vector ()
  "yeetube--image-callback stores the image display property on the content vector."
  (let* ((fake-image (list 'image :type 'png :data "fakedata"))
         (vec (vector "[[test-id.jpg]]" "Title" "100" "1:00" "1 day ago" "Ch"))
         (yeetube-content (list (list "test-id" vec)))
         (yeetube-thumbnail-size '(120 . 90))
         (entry (list "test-id" "[[test-id.jpg]]" "Title" "100" "1:00" "1 day ago" "Ch")))
    ;; Create the display buffer with placeholder text
    (let ((display-buf (generate-new-buffer " *yeetube-test*")))
      (unwind-protect
          (progn
            (with-current-buffer display-buf
              (insert "[[test-id.jpg]]"))
            (cl-letf (((symbol-function 'mm-dissect-buffer) (lambda (&rest _) t))
                      ((symbol-function 'mm-get-image) (lambda (_) fake-image))
                      ((symbol-function 'image-property)
                       (lambda (img prop) (plist-get (cdr img) prop)))
                      ((symbol-function 'set-image-property)
                       (lambda (img prop val) (plist-put (cdr img) prop val))))
              ;; Call from a temp buffer simulating the url-retrieve callback
              (with-temp-buffer
                (yeetube--image-callback nil entry display-buf)))
            ;; The vector's thumbnail slot should now carry a display property
            (should (get-text-property 0 'display (aref vec 0))))
        (kill-buffer display-buf)))))

(ert-deftest yeetube-test-image-callback-displays-image-in-buffer ()
  "yeetube--image-callback places the image in the display buffer.
The buffer argument is passed as a string name, matching real usage."
  (let* ((fake-image (list 'image :type 'png :data "fakedata"))
         (vec (vector "[[test-id.jpg]]" "Title" "100" "1:00" "1 day ago" "Ch"))
         (yeetube-content (list (list "test-id" vec)))
         (yeetube-thumbnail-size '(120 . 90))
         (entry (list "test-id" "[[test-id.jpg]]" "Title" "100" "1:00" "1 day ago" "Ch"))
         (buf-name " *yeetube-display-test*"))
    (let ((display-buf (generate-new-buffer buf-name)))
      (unwind-protect
          (progn
            (with-current-buffer display-buf
              (insert "[[test-id.jpg]]"))
            (cl-letf (((symbol-function 'mm-dissect-buffer) (lambda (&rest _) t))
                      ((symbol-function 'mm-get-image) (lambda (_) fake-image))
                      ((symbol-function 'image-property)
                       (lambda (img prop) (plist-get (cdr img) prop)))
                      ((symbol-function 'set-image-property)
                       (lambda (img prop val) (plist-put (cdr img) prop val))))
              ;; Pass buffer as a STRING name, just like the real code does
              (with-temp-buffer
                (yeetube--image-callback nil entry buf-name)))
            ;; The placeholder text in the buffer should now have a display property
            (with-current-buffer display-buf
              (should (get-text-property 1 'display (buffer-string)))))
        (when (buffer-live-p display-buf)
          (kill-buffer display-buf))))))

(ert-deftest yeetube-test-image-callback-no-crash-on-missing-entry ()
  "yeetube--image-callback does not crash when entry is not in yeetube-content."
  (let* ((fake-image (list 'image :type 'png :data "fakedata"))
         (yeetube-content nil)
         (yeetube-thumbnail-size '(120 . 90))
         (entry (list "nonexistent" "[[nonexistent.jpg]]" "Title")))
    (with-temp-buffer
      (insert "[[nonexistent.jpg]]")
      (let ((target-buf (current-buffer)))
        (cl-letf (((symbol-function 'mm-dissect-buffer) (lambda (&rest _) t))
                  ((symbol-function 'mm-get-image) (lambda (_) fake-image))
                  ((symbol-function 'image-property)
                   (lambda (img prop) (plist-get (cdr img) prop)))
                  ((symbol-function 'set-image-property)
                   (lambda (img prop val) (plist-put (cdr img) prop val))))
          ;; Should not signal an error
          (with-temp-buffer
            (yeetube--image-callback nil entry target-buf)))))))

(provide 'yeetube-tests)
;;; yeetube-tests.el ends here
