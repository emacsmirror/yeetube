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

;;; Group 1: yeetube-get-filter-code

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

;;; Group 2: yeetube--callback error detection (regression)

(ert-deftest yeetube-test-callback-plist-get-arg-order ()
  "Verify plist-get extracts :error from a status plist correctly."
  ;; This is the core of the bug: (plist-get :error status) always returned nil.
  (let ((status '(:error (error http 404))))
    (should (equal '(error http 404) (plist-get status :error))))
  ;; No error case
  (let ((status '(:peer (:certificate ...))))
    (should-not (plist-get status :error))))

;;; Group 3: yeetube--find-item

(ert-deftest yeetube-test-find-item ()
  "Find item plist by ID."
  (let ((yeetube-items '((:id "abc" :title "First" :type video)
                         (:id "def" :title "Second" :type video))))
    (should (equal "First" (plist-get (yeetube--find-item "abc") :title)))
    (should (equal "Second" (plist-get (yeetube--find-item "def") :title)))
    (should-not (yeetube--find-item "nonexistent"))))

;;; Group 4: yeetube--normalize-channel-id

(ert-deftest yeetube-test-normalize-channel-id-path-handle ()
  "Normalize canonical path channel handles."
  (should (string= "foo" (yeetube--normalize-channel-id "/@foo"))))

(ert-deftest yeetube-test-normalize-channel-id-handle ()
  "Normalize channel handles."
  (should (string= "foo" (yeetube--normalize-channel-id "@foo"))))

(ert-deftest yeetube-test-normalize-channel-id-channel-path ()
  "Normalize channel paths."
  (should (string= "UCabc" (yeetube--normalize-channel-id "/channel/UCabc"))))

(ert-deftest yeetube-test-normalize-channel-id-plain ()
  "Plain channel ids are returned unchanged."
  (should (string= "foo" (yeetube--normalize-channel-id "foo"))))

;;; Group 5: yeetube-get-url

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

;;; Group 6: yeetube-mpv-play returns a process (regression)

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

;;; Group 7: yeetube-save-saved-videos round-trip (regression)

(ert-deftest yeetube-test-save-saved-videos-round-trip ()
  "Saved videos can be written and read back."
  (let* ((temp-dir (make-temp-file "yeetube-test" t))
         (user-emacs-directory (file-name-as-directory temp-dir))
         (yeetube-saved-videos '(("Video One" . "https://youtube.com/watch?v=abc")
                                 ("Video Two" . "https://youtube.com/watch?v=def"))))
    (unwind-protect
        (progn
          (yeetube-save-saved-videos)
          (setf yeetube-saved-videos nil)
          (yeetube-load-saved-videos)
          (should (equal '(("Video One" . "https://youtube.com/watch?v=abc")
                           ("Video Two" . "https://youtube.com/watch?v=def"))
                         yeetube-saved-videos)))
      (delete-directory temp-dir t))))

;;; Group 8: yeetube-mode-map keybindings

(ert-deftest yeetube-test-keymap-s-bound-to-search ()
  "Lowercase s is bound to yeetube-search."
  (should (eq 'yeetube-search (lookup-key yeetube-mode-map "s"))))

(ert-deftest yeetube-test-keymap-S-bound-to-save-video ()
  "S key is bound to yeetube-save-video."
  (should (eq 'yeetube-save-video (lookup-key yeetube-mode-map "S"))))

(ert-deftest yeetube-test-keymap-L-bound-to-channel-streams ()
  "L key is bound to yeetube-channel-streams."
  (should (eq 'yeetube-channel-streams (lookup-key yeetube-mode-map "L"))))

(ert-deftest yeetube-test-keymap-R-bound-to-rss-feed ()
  "R key is bound to yeetube-copy-rss-feed-url."
  (should (eq 'yeetube-copy-rss-feed-url (lookup-key yeetube-mode-map "R"))))

(ert-deftest yeetube-test-keymap-M-n-bound-to-next-page ()
  "M-n is bound to yeetube-next-page."
  (should (eq 'yeetube-next-page (lookup-key yeetube-mode-map (kbd "M-n")))))

;;; Group 9: yeetube--channel-url & channel-videos

(ert-deftest yeetube-test-channel-url-handle ()
  "Channel URLs are built without duplicate slashes for handles."
  (should (equal "https://youtube.com/@foo/videos?ucbcb=1"
                 (yeetube--channel-url "@foo" "videos?ucbcb=1"))))

(ert-deftest yeetube-test-channel-url-channel-path ()
  "Channel URLs are built without duplicate slashes for channel paths."
  (should (equal "https://youtube.com/channel/UCsystemcrafters/streams?ucbcb=1"
                 (yeetube--channel-url "/channel/UCsystemcrafters"
                                       "streams?ucbcb=1"))))

(ert-deftest yeetube-test-channel-url-search ()
  "Channel search URLs include the query without duplicate slashes."
  (should (equal "https://youtube.com/@foo/search?query=bar&ucbcb=1"
                 (yeetube--channel-url "@foo"
                                       "search?query=bar&ucbcb=1"))))

(ert-deftest yeetube-test-channel-videos-scrapes ()
  "Channel videos scrapes the channel videos page."
  (let (scrape-url)
    (unwind-protect
        (cl-letf (((symbol-function 'yeetube-display-content-from-url)
                   (lambda (url)
                     (setq scrape-url url))))
          (yeetube-channel-videos "/channel/UCsystemcrafters"))
      (when (get-buffer "*yeetube*")
        (kill-buffer "*yeetube*")))
    (should (equal "https://youtube.com/channel/UCsystemcrafters/videos?ucbcb=1"
                   scrape-url))))

;;; Group 10: yeetube RSS parsing

(defconst yeetube-test-rss-feed
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:yt=\"http://www.youtube.com/xml/schemas/2015\">
  <entry>
    <yt:videoId>abc123</yt:videoId>
    <yt:channelId>UCsystemcrafters</yt:channelId>
    <title>First video</title>
    <published>2026-05-01T12:00:00+00:00</published>
    <updated>2026-05-02T12:00:00+00:00</updated>
    <author>
      <name>System Crafters</name>
      <uri>https://www.youtube.com/channel/UCsystemcrafters</uri>
    </author>
  </entry>
  <entry>
    <yt:videoId>def456</yt:videoId>
    <yt:channelId>UCsystemcrafters</yt:channelId>
    <title>Second video</title>
    <updated>2026-05-03T12:00:00+00:00</updated>
    <author>
      <name>System Crafters</name>
    </author>
  </entry>
</feed>")

(defun yeetube-test--rss-callback-fallback-url (status fallback-url &optional contents)
  "Call `yeetube--rss-callback' with STATUS, FALLBACK-URL and CONTENTS."
  (let ((buffer (generate-new-buffer " *yeetube-rss-test*"))
        captured-url)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (when contents
              (insert contents))
            (cl-letf (((symbol-function 'yeetube-display-content-from-url)
                       (lambda (url)
                         (setq captured-url url))))
              (yeetube--rss-callback status fallback-url)))
          captured-url)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yeetube-test-rss-callback-falls-back-on-retrieval-error ()
  "RSS callback displays the fallback URL when retrieval fails."
  (should (equal "https://youtube.com/@systemcrafters/videos?ucbcb=1"
                 (yeetube-test--rss-callback-fallback-url
                  '(:error (error http 500))
                  "https://youtube.com/@systemcrafters/videos?ucbcb=1"))))

(ert-deftest yeetube-test-rss-callback-falls-back-on-malformed-xml ()
  "RSS callback displays the fallback URL when XML parsing fails."
  (should (equal "https://youtube.com/@systemcrafters/videos?ucbcb=1"
                 (yeetube-test--rss-callback-fallback-url
                  nil
                  "https://youtube.com/@systemcrafters/videos?ucbcb=1"
                  "<feed><entry>"))))

(ert-deftest yeetube-test-rss-callback-falls-back-on-empty-feed ()
  "RSS callback displays the fallback URL when the feed has no videos."
  (should (equal "https://youtube.com/@systemcrafters/videos?ucbcb=1"
                 (yeetube-test--rss-callback-fallback-url
                  nil
                  "https://youtube.com/@systemcrafters/videos?ucbcb=1"
                  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:yt=\"http://www.youtube.com/xml/schemas/2015\">
</feed>"))))

(ert-deftest yeetube-test-rss-parse-buffer-converts-videos ()
  "RSS entries are converted to yeetube item plists."
  (let ((items (with-temp-buffer
                 (insert yeetube-test-rss-feed)
                 (yeetube--rss-parse-buffer))))
    (should (= 2 (length items)))
    (let ((item (car items)))
      (should (equal "abc123" (plist-get item :id)))
      (should (equal "First video" (plist-get item :title)))
      (should (equal "System Crafters" (plist-get item :channel)))
      (should (equal "/channel/UCsystemcrafters" (plist-get item :channel-id)))
      (should (equal "UCsystemcrafters" (plist-get item :browse-id)))
      (should (equal "https://i.ytimg.com/vi/abc123/default.jpg"
                     (plist-get item :thumbnail-url)))
      (should (equal "" (plist-get item :views)))
      (should (equal "" (plist-get item :duration)))
      (should (equal "2026-05-01T12:00:00+00:00" (plist-get item :date)))
      (should (eq 'video (plist-get item :type))))
    (should (equal "def456" (plist-get (cadr items) :id)))
    (should (equal "2026-05-03T12:00:00+00:00"
                   (plist-get (cadr items) :date)))
    (should (equal "" (plist-get (cadr items) :views)))
    (should (equal "" (plist-get (cadr items) :duration)))))

(ert-deftest yeetube-test-rss-parse-buffer-filters-entries-without-video-id ()
  "RSS entries without video IDs are ignored."
  (let ((items (with-temp-buffer
                 (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:yt=\"http://www.youtube.com/xml/schemas/2015\">
  <entry>
    <yt:channelId>UCsystemcrafters</yt:channelId>
    <title>Missing video ID</title>
  </entry>
  <entry>
    <yt:videoId>abc123</yt:videoId>
    <yt:channelId>UCsystemcrafters</yt:channelId>
    <title>Valid video</title>
  </entry>
</feed>")
                 (yeetube--rss-parse-buffer))))
    (should (= 1 (length items)))
    (should (equal "abc123" (plist-get (car items) :id)))))

(ert-deftest yeetube-test-rss-entry-views-from-media-statistics ()
  "RSS :views is populated from media:statistics views attribute."
  (let ((items (with-temp-buffer
                 (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\"
      xmlns:yt=\"http://www.youtube.com/xml/schemas/2015\"
      xmlns:media=\"http://search.yahoo.com/mrss/\">
  <entry>
    <yt:videoId>abc123</yt:videoId>
    <yt:channelId>UCsystemcrafters</yt:channelId>
    <title>Stats</title>
    <media:group>
      <media:statistics views=\"4242\"/>
    </media:group>
  </entry>
</feed>")
                 (yeetube--rss-parse-buffer))))
    (should (equal "4242" (plist-get (car items) :views)))))

(ert-deftest yeetube-test-rss-entry-thumbnail-from-media-thumbnail ()
  "RSS :thumbnail-url uses media:thumbnail and rewrites hqdefault."
  (let ((items (with-temp-buffer
                 (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\"
      xmlns:yt=\"http://www.youtube.com/xml/schemas/2015\"
      xmlns:media=\"http://search.yahoo.com/mrss/\">
  <entry>
    <yt:videoId>abc123</yt:videoId>
    <yt:channelId>UCsystemcrafters</yt:channelId>
    <title>Thumb</title>
    <media:group>
      <media:thumbnail url=\"https://i.ytimg.com/vi/abc123/hqdefault.jpg\"
                       width=\"480\" height=\"360\"/>
    </media:group>
  </entry>
</feed>")
                 (yeetube--rss-parse-buffer))))
    (should (equal "https://i.ytimg.com/vi/abc123/mqdefault.jpg"
                   (plist-get (car items) :thumbnail-url)))))

;;; Group 11: yeetube-mode buffer-local settings

(ert-deftest yeetube-test-mode-sets-truncate-string-ellipsis ()
  "yeetube-mode sets truncate-string-ellipsis to a single space."
  (let ((yeetube-content nil)
        (yeetube-display-thumbnails-p nil))
    (with-temp-buffer
      (yeetube-mode)
      (should (string= " " truncate-string-ellipsis)))))

(provide 'yeetube-tests)
;;; yeetube-tests.el ends here
