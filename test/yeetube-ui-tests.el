;;; yeetube-ui-tests.el --- Tests for yeetube-ui  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Run: emacs -Q --batch -L .. -l test/yeetube-ui-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(unless (boundp 'find-function-mode)
  (defvar find-function-mode nil))

(require 'ert)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'yeetube)

;;; Group 1: yeetube-ui--format-views

(ert-deftest yeetube-ui-test-format-views-empty ()
  "Empty string returns empty."
  (should (string= "" (yeetube-ui--format-views ""))))

(ert-deftest yeetube-ui-test-format-views-zero ()
  "A genuine zero count remains visible."
  (should (string= "0" (yeetube-ui--format-views "0 views"))))

(ert-deftest yeetube-ui-test-format-views-single-digit ()
  "Single digit has no commas."
  (should (string= "5" (yeetube-ui--format-views "5"))))

(ert-deftest yeetube-ui-test-format-views-hundreds ()
  "Hundreds have no commas."
  (should (string= "999" (yeetube-ui--format-views "999"))))

(ert-deftest yeetube-ui-test-format-views-thousands ()
  "Thousands get one comma."
  (should (string= "1,000" (yeetube-ui--format-views "1000"))))

(ert-deftest yeetube-ui-test-format-views-millions ()
  "Millions get two commas."
  (should (string= "1,234,567" (yeetube-ui--format-views "1234567"))))

(ert-deftest yeetube-ui-test-format-views-with-text ()
  "Non-digit characters are stripped before formatting."
  (should (string= "1,234" (yeetube-ui--format-views "1,234 views"))))

(ert-deftest yeetube-ui-test-format-views-abbreviations ()
  "Abbreviated view counts retain their magnitude."
  (should (string= "1,200" (yeetube-ui--format-views "1.2K")))
  (should (string= "3,400,000" (yeetube-ui--format-views "3.4M views"))))

;;; Group 2: yeetube-ui--duration-to-seconds

(ert-deftest yeetube-ui-test-duration-to-seconds-hhmmss ()
  "HH:MM:SS format converts correctly."
  (should (= 3661 (yeetube-ui--duration-to-seconds "1:01:01"))))

(ert-deftest yeetube-ui-test-duration-to-seconds-mmss ()
  "MM:SS format converts correctly."
  (should (= 125 (yeetube-ui--duration-to-seconds "2:05"))))

(ert-deftest yeetube-ui-test-duration-to-seconds-ss ()
  "SS-only format converts correctly."
  (should (= 45 (yeetube-ui--duration-to-seconds "45"))))

(ert-deftest yeetube-ui-test-duration-to-seconds-zero ()
  "Zero duration."
  (should (= 0 (yeetube-ui--duration-to-seconds "0:00"))))

(ert-deftest yeetube-ui-test-duration-to-seconds-large ()
  "Large duration converts correctly."
  (should (= 36610 (yeetube-ui--duration-to-seconds "10:10:10"))))

;;; Group 3: yeetube-ui--parse-relative-date

(ert-deftest yeetube-ui-test-parse-relative-date-seconds ()
  "Seconds parsed correctly."
  (should (= 30 (yeetube-ui--parse-relative-date "30 seconds ago"))))

(ert-deftest yeetube-ui-test-parse-relative-date-minutes ()
  "Minutes parsed correctly."
  (should (= 300 (yeetube-ui--parse-relative-date "5 minutes ago"))))

(ert-deftest yeetube-ui-test-parse-relative-date-hours ()
  "Hours parsed correctly."
  (should (= 7200 (yeetube-ui--parse-relative-date "2 hours ago"))))

(ert-deftest yeetube-ui-test-parse-relative-date-days ()
  "Days parsed correctly."
  (should (= 259200 (yeetube-ui--parse-relative-date "3 days ago"))))

(ert-deftest yeetube-ui-test-parse-relative-date-weeks ()
  "Weeks parsed correctly."
  (should (= 604800 (yeetube-ui--parse-relative-date "1 week ago"))))

(ert-deftest yeetube-ui-test-parse-relative-date-months ()
  "Months parsed correctly (30-day month)."
  (should (= 5184000 (yeetube-ui--parse-relative-date "2 months ago"))))

(ert-deftest yeetube-ui-test-parse-relative-date-years ()
  "Years parsed correctly (365-day year)."
  (should (= 31536000 (yeetube-ui--parse-relative-date "1 year ago"))))

(ert-deftest yeetube-ui-test-parse-relative-date-unknown-unit ()
  "Unknown unit returns 0."
  (should (= 0 (yeetube-ui--parse-relative-date "5 fortnights ago"))))

(ert-deftest yeetube-ui-test-parse-relative-date-iso ()
  "ISO-8601 timestamps parse to nonzero epoch seconds."
  (should (< 0 (yeetube-ui--parse-relative-date "2026-05-01T12:00:00+00:00"))))

(ert-deftest yeetube-ui-test-parse-relative-date-iso-ordered ()
  "Older ISO timestamps produce smaller values than newer ones."
  (should (< (yeetube-ui--parse-relative-date "2020-01-01T00:00:00+00:00")
             (yeetube-ui--parse-relative-date "2026-05-01T12:00:00+00:00"))))

;;; Group 4: yeetube-ui--entry-to-row

(ert-deftest yeetube-ui-test-entry-to-row-video-no-thumbnails ()
  "Plist converts to row without thumbnail column."
  (let* ((yeetube-display-thumbnails-p nil)
         (entry '(:id "abc" :title "Test Title" :views "1000"
                      :duration "3:00" :date "1 day ago"
                      :channel "TestCh" :channel-id "/@testch" :type video))
         (row (yeetube-ui--entry-to-row entry))
         (id (car row))
         (vec (cadr row)))
    (should (equal "abc" id))
    ;; title=0, views=1, duration=2, date=3, channel=4
    (should (= 5 (length vec)))
    (should (string-match-p "Test Title" (aref vec 0)))
    (should (string-match-p "1,000" (aref vec 1)))
    (should (string-match-p "3:00" (aref vec 2)))
    (should (string-match-p "1 day ago" (aref vec 3)))
    (should (string-match-p "TestCh" (aref vec 4)))))

(ert-deftest yeetube-ui-test-entry-to-row-video-with-thumbnails ()
  "Plist converts to row with thumbnail placeholder."
  (let* ((yeetube-display-thumbnails-p t)
         (entry '(:id "abc" :title "Test Title" :views "1000"
                      :duration "3:00" :date "1 day ago"
                      :channel "TestCh" :channel-id "/@testch" :type video))
         (row (yeetube-ui--entry-to-row entry))
         (vec (cadr row)))
    ;; thumbnail=0, title=1, views=2, duration=3, date=4, channel=5
    (should (string= "[[abc.jpg]]" (aref vec 0)))
    (should (string-match-p "Test Title" (aref vec 1)))
    (should (string-match-p "1,000" (aref vec 2)))))

(ert-deftest yeetube-ui-test-entry-to-row-playlist-prefix ()
  "Playlist entries get a \"Playlist: \" prefix in the title."
  (let* ((yeetube-display-thumbnails-p nil)
         (entry '(:id "PLxyz" :title "My List" :views "" :duration ""
                      :date "" :channel "Ch" :channel-id "/@ch" :type playlist))
         (row (yeetube-ui--entry-to-row entry))
         (vec (cadr row)))
    (should (string-match-p "Playlist: My List" (aref vec 0)))))

;;; Group 5: Sort functions

(ert-deftest yeetube-ui-test-sort-views-with-thumbnails ()
  "Sort by views works when thumbnails are enabled (index 2)."
  (let ((yeetube-display-thumbnails-p t)
        (a '("id1" ["thumb" "Title A" "1,000" "3:00" "1 day ago" "Ch"]))
        (b '("id2" ["thumb" "Title B" "2,000" "5:00" "2 days ago" "Ch"])))
    (should (yeetube-ui--sort-views a b))
    (should-not (yeetube-ui--sort-views b a))))

(ert-deftest yeetube-ui-test-sort-views-without-thumbnails ()
  "Sort by views works when thumbnails are disabled (index 1)."
  (let ((yeetube-display-thumbnails-p nil)
        (a '("id1" ["Title A" "1,000" "3:00" "1 day ago" "Ch"]))
        (b '("id2" ["Title B" "2,000" "5:00" "2 days ago" "Ch"])))
    (should (yeetube-ui--sort-views a b))
    (should-not (yeetube-ui--sort-views b a))))

(ert-deftest yeetube-ui-test-sort-views-abbreviation ()
  "Abbreviated counts sort by magnitude."
  (let ((yeetube-display-thumbnails-p nil)
        (a '("id1" ["Title A" "999" "3:00" "1 day ago" "Ch"]))
        (b '("id2" ["Title B" "1.2K" "5:00" "2 days ago" "Ch"])))
    (should (yeetube-ui--sort-views a b))
    (should-not (yeetube-ui--sort-views b a))))

(ert-deftest yeetube-ui-test-sort-views-zero ()
  "Zero sorts below positive view counts."
  (let ((yeetube-display-thumbnails-p nil)
        (zero '("id1" ["Title A" "0" "3:00" "1 day ago" "Ch"]))
        (positive '("id2" ["Title B" "1" "5:00" "2 days ago" "Ch"])))
    (should (yeetube-ui--sort-views zero positive))
    (should-not (yeetube-ui--sort-views positive zero))))

(ert-deftest yeetube-ui-test-sort-duration-with-thumbnails ()
  "Sort by duration works when thumbnails are enabled (index 3)."
  (let ((yeetube-display-thumbnails-p t)
        (a '("id1" ["thumb" "Title A" "100" "1:00" "1 day ago" "Ch"]))
        (b '("id2" ["thumb" "Title B" "200" "2:00" "2 days ago" "Ch"])))
    (should (yeetube-ui--sort-duration a b))
    (should-not (yeetube-ui--sort-duration b a))))

(ert-deftest yeetube-ui-test-sort-duration-without-thumbnails ()
  "Sort by duration works when thumbnails are disabled (index 2)."
  (let ((yeetube-display-thumbnails-p nil)
        (a '("id1" ["Title A" "100" "1:00" "1 day ago" "Ch"]))
        (b '("id2" ["Title B" "200" "2:00" "2 days ago" "Ch"])))
    (should (yeetube-ui--sort-duration a b))
    (should-not (yeetube-ui--sort-duration b a))))

(ert-deftest yeetube-ui-test-sort-date-with-thumbnails ()
  "Sort by date works when thumbnails are enabled (index 4)."
  (let ((yeetube-display-thumbnails-p t)
        (a '("id1" ["thumb" "Title A" "100" "1:00" "1 day ago" "Ch"]))
        (b '("id2" ["thumb" "Title B" "200" "2:00" "2 days ago" "Ch"])))
    (should (yeetube-ui--sort-date a b))
    (should-not (yeetube-ui--sort-date b a))))

(ert-deftest yeetube-ui-test-sort-date-without-thumbnails ()
  "Sort by date works when thumbnails are disabled (index 3)."
  (let ((yeetube-display-thumbnails-p nil)
        (a '("id1" ["Title A" "100" "1:00" "1 day ago" "Ch"]))
        (b '("id2" ["Title B" "200" "2:00" "2 days ago" "Ch"])))
    (should (yeetube-ui--sort-date a b))
    (should-not (yeetube-ui--sort-date b a))))

;;; Group 6: Thumbnail image callback

(ert-deftest yeetube-ui-test-image-callback-persists-image-on-vector ()
  "yeetube-ui--image-callback stores the image display property on the content vector."
  (let* ((fake-image (list 'image :type 'png :data "fakedata"))
         (vec (vector "[[test-id.jpg]]" "Title" "100" "1:00" "1 day ago" "Ch"))
         (yeetube-content (list (list "test-id" vec)))
         (yeetube-thumbnail-size '(120 . 90)))
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
              (with-temp-buffer
                (yeetube-ui--image-callback nil "test-id" display-buf)))
            ;; The vector's thumbnail slot should now carry a display property
            (should (get-text-property 0 'display (aref vec 0))))
        (kill-buffer display-buf)))))

(ert-deftest yeetube-ui-test-image-callback-displays-image-in-buffer ()
  "yeetube-ui--image-callback places the image in the display buffer."
  (let* ((fake-image (list 'image :type 'png :data "fakedata"))
         (vec (vector "[[test-id.jpg]]" "Title" "100" "1:00" "1 day ago" "Ch"))
         (yeetube-content (list (list "test-id" vec)))
         (yeetube-thumbnail-size '(120 . 90))
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
                (yeetube-ui--image-callback nil "test-id" buf-name)))
            ;; The placeholder text in the buffer should now have a display property
            (with-current-buffer display-buf
              (should (get-text-property 1 'display (buffer-string)))))
        (when (buffer-live-p display-buf)
          (kill-buffer display-buf))))))

(ert-deftest yeetube-ui-test-image-callback-no-crash-on-missing-entry ()
  "yeetube-ui--image-callback does not crash when entry is not in yeetube-content."
  (let* ((fake-image (list 'image :type 'png :data "fakedata"))
         (yeetube-content nil)
         (yeetube-thumbnail-size '(120 . 90)))
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
            (yeetube-ui--image-callback nil "nonexistent" target-buf)))))))

(provide 'yeetube-ui-tests)
;;; yeetube-ui-tests.el ends here
