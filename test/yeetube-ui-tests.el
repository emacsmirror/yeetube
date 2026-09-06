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

(ert-deftest yeetube-ui-test-duration-to-seconds-playlist-count ()
  "Playlist video-count labels are not treated as HMS seconds."
  (should (= 0 (yeetube-ui--duration-to-seconds "3 videos")))
  (should (= 0 (yeetube-ui--duration-to-seconds "15 videos"))))

(ert-deftest yeetube-ui-test-duration-to-seconds-live-and-empty ()
  "LIVE and empty duration labels sort as unknown (0)."
  (should (= 0 (yeetube-ui--duration-to-seconds "LIVE")))
  (should (= 0 (yeetube-ui--duration-to-seconds ""))))

(ert-deftest yeetube-ui-test-sort-duration-playlist-after-unknown ()
  "Playlist count does not sort ahead of multi-minute videos."
  (let ((yeetube-display-thumbnails-p nil)
        (playlist '("pl" ["P" "1" "3 videos" "1 day ago" "Ch"]))
        (video '("v" ["V" "1" "3:00" "1 day ago" "Ch"])))
    (should (yeetube-ui--sort-duration playlist video))
    (should-not (yeetube-ui--sort-duration video playlist))))

;;; Group 3: yeetube-ui--parse-relative-date
;;; Polarity: older-before-newer for relative, ISO, and mixed lists.

(ert-deftest yeetube-ui-test-parse-relative-date-relative-ordered ()
  "Relative ages share older-before-newer chronology."
  (should (< (yeetube-ui--parse-relative-date "5 days ago")
             (yeetube-ui--parse-relative-date "1 day ago")))
  (should (< (yeetube-ui--parse-relative-date "2 hours ago")
             (yeetube-ui--parse-relative-date "30 seconds ago")))
  (should (< (yeetube-ui--parse-relative-date "1 year ago")
             (yeetube-ui--parse-relative-date "1 week ago"))))

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

(ert-deftest yeetube-ui-test-parse-relative-date-mixed-chronology ()
  "Mixed relative and ISO dates share one older-before-newer scale."
  (let ((old-rel (yeetube-ui--parse-relative-date "10 years ago"))
        (mid-iso (yeetube-ui--parse-relative-date "2020-01-01T00:00:00+00:00"))
        (late-iso (yeetube-ui--parse-relative-date "2025-01-01T00:00:00+00:00"))
        (new-rel (yeetube-ui--parse-relative-date "1 day ago")))
    (should (< old-rel mid-iso))
    (should (< mid-iso late-iso))
    (should (< late-iso new-rel))))

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
  "Sort by date is older-before-newer when thumbnails are enabled."
  (let ((yeetube-display-thumbnails-p t)
        (newer '("id1" ["thumb" "Title A" "100" "1:00" "1 day ago" "Ch"]))
        (older '("id2" ["thumb" "Title B" "200" "2:00" "2 days ago" "Ch"])))
    (should (yeetube-ui--sort-date older newer))
    (should-not (yeetube-ui--sort-date newer older))))

(ert-deftest yeetube-ui-test-sort-date-without-thumbnails ()
  "Sort by date is older-before-newer when thumbnails are disabled."
  (let ((yeetube-display-thumbnails-p nil)
        (newer '("id1" ["Title A" "100" "1:00" "1 day ago" "Ch"]))
        (older '("id2" ["Title B" "200" "2:00" "2 days ago" "Ch"])))
    (should (yeetube-ui--sort-date older newer))
    (should-not (yeetube-ui--sort-date newer older))))

(ert-deftest yeetube-ui-test-sort-date-mixed-iso-relative ()
  "Date sort unifies ISO and relative values with older-before-newer."
  (let ((yeetube-display-thumbnails-p nil)
        (iso-old '("id1" ["A" "100" "1:00" "2020-01-01T00:00:00+00:00" "Ch"]))
        (rel-new '("id2" ["B" "200" "2:00" "1 day ago" "Ch"])))
    (should (yeetube-ui--sort-date iso-old rel-new))
    (should-not (yeetube-ui--sort-date rel-new iso-old))))

(ert-deftest yeetube-ui-test-sort-date-equal-relative ()
  "Equal relative dates do not compare less in either direction."
  (let ((yeetube-display-thumbnails-p nil)
        (a '("id1" ["A" "100" "1:00" "1 day ago" "Ch"]))
        (b '("id2" ["B" "200" "2:00" "1 day ago" "Ch"])))
    (should-not (yeetube-ui--sort-date a b))
    (should-not (yeetube-ui--sort-date b a))))

(ert-deftest yeetube-ui-test-default-sort-column-includes-date ()
  "Customize type offers Date alongside existing columns."
  (let ((type (get 'yeetube-default-sort-column 'custom-type)))
    (should (eq (car type) 'radio))
    (should (member '(const "Date") (cdr type)))
    (should (member '(const "Title") (cdr type)))
    (should (member '(const "Views") (cdr type)))
    (should (member '(const "Duration") (cdr type)))
    (should (member '(const "Channel") (cdr type)))))

(ert-deftest yeetube-ui-test-render-default-sort-date ()
  "Default Date sort honors ascending and descending user values."
  (dolist (case '((t ("old" "new"))
                  (nil ("new" "old"))))
    (let ((yeetube-default-sort-column "Date")
          (yeetube-default-sort-ascending (car case))
          (yeetube-display-thumbnails-p nil)
          (yeetube-content nil))
      (with-temp-buffer
        (tabulated-list-mode)
        (yeetube-ui-render
         (list '(:id "new" :title "New" :views "1" :duration "1:00"
                     :date "1 day ago" :channel "C" :type video)
               '(:id "old" :title "Old" :views "1" :duration "1:00"
                     :date "2 days ago" :channel "C" :type video)))
        (should (equal (mapcar #'car tabulated-list-entries) (cadr case)))))))

;;; Group 6: Thumbnail image callback

(defun yeetube-ui-test--thumbnail-result (&optional remove-row)
  "Deliver a deferred thumbnail, optionally after REMOVE-ROW.
Return the display properties of the row vector and placeholder text."
  (let ((yeetube-display-thumbnails-p t)
        (item '(:id "test-id" :title "Title" :views "100" :duration "1:00"
                :date "1 day ago" :channel "Ch" :type video
                :thumbnail-url "https://invalid/image"))
        callback args)
    (with-temp-buffer
      (yeetube-mode)
      (yeetube-ui-render (list item))
      (cl-letf (((symbol-function 'yeetube--queue-retrieve)
                 (lambda (_url function cbargs)
                   (setq callback function args cbargs)))
                ((symbol-function 'yeetube-ui--extract-image)
                 (lambda (_) '(image :type png :data "fakedata"))))
        (yeetube-ui-fetch-thumbnails (list item) (buffer-name))
        (let ((row (car yeetube-content)))
          (when remove-row (setq yeetube-content nil))
          (with-temp-buffer (apply callback nil args))
          (goto-char (point-min))
          (search-forward "[[test-id.jpg]]")
          (cons (get-text-property 0 'display (aref (cadr row) 0))
                (get-text-property (match-beginning 0) 'display)))))))

(ert-deftest yeetube-ui-test-image-callback-persists-image-on-vector ()
  "Deferred thumbnails persist on the owned row vector."
  (should (equal '(image :type png :data "fakedata")
                 (car (yeetube-ui-test--thumbnail-result)))))

(ert-deftest yeetube-ui-test-image-callback-displays-image-in-buffer ()
  "Deferred thumbnails decorate the owned row's placeholder."
  (should (equal '(image :type png :data "fakedata")
                 (cdr (yeetube-ui-test--thumbnail-result)))))

(ert-deftest yeetube-ui-test-image-callback-no-crash-on-missing-entry ()
  "Removing a queued thumbnail's row prevents vector and text mutation."
  (should (equal '(nil) (yeetube-ui-test--thumbnail-result t))))
(provide 'yeetube-ui-tests)
;;; yeetube-ui-tests.el ends here
