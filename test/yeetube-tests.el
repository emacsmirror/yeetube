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

;;; Group 1: yeetube--page-callback error detection (regression)

(ert-deftest yeetube-test-callback-plist-get-arg-order ()
  "Verify plist-get extracts :error from a status plist correctly."
  ;; This is the core of the bug: (plist-get :error status) always returned nil.
  (let ((status '(:error (error http 404))))
    (should (equal '(error http 404) (plist-get status :error))))
  ;; No error case
  (let ((status '(:peer (:certificate ...))))
    (should-not (plist-get status :error))))

;;; Group 2: yeetube--find-item

(ert-deftest yeetube-test-find-item ()
  "Find item plist by ID."
  (let ((yeetube-items '((:id "abc" :title "First" :type video)
                         (:id "def" :title "Second" :type video))))
    (should (equal "First" (plist-get (yeetube--find-item "abc") :title)))
    (should (equal "Second" (plist-get (yeetube--find-item "def") :title)))
    (should-not (yeetube--find-item "nonexistent"))))

;;; Group 3: yeetube-get-url

(ert-deftest yeetube-test-get-url-video ()
  "Get URL for a video."
  (let ((yeetube-youtube-video-url "https://youtube.com/watch?v=")
        (yeetube-items '((:id "abc" :type video))))
    (should (equal "https://youtube.com/watch?v=abc"
                   (yeetube-get-url "abc" 'video)))))

(ert-deftest yeetube-test-get-url-playlist ()
  "Get URL for a playlist."
  (let ((yeetube-youtube-playlist-url "https://youtube.com/playlist?list="))
    (should (equal "https://youtube.com/playlist?list=PLxyz"
                   (yeetube-get-url "PLxyz" 'playlist)))))

;;; Group 4: yeetube-mpv-play returns a process (regression)

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

;;; Group 5: yeetube-save-saved-videos round-trip (regression)

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

(ert-deftest yeetube-test-load-saved-videos-empty-file ()
  "An empty bookmark file yields nil without signaling."
  (let* ((temp-dir (make-temp-file "yeetube-test" t))
         (user-emacs-directory (file-name-as-directory temp-dir))
         (yeetube-saved-videos nil))
    (unwind-protect
        (progn
          (write-region "" nil (locate-user-emacs-file "yeetube"))
          (yeetube-load-saved-videos)
          (should (null yeetube-saved-videos)))
      (delete-directory temp-dir t))))

(ert-deftest yeetube-test-load-saved-videos-corrupt-file ()
  "A malformed bookmark file yields nil without signaling."
  (let* ((temp-dir (make-temp-file "yeetube-test" t))
         (user-emacs-directory (file-name-as-directory temp-dir))
         (yeetube-saved-videos nil))
    (unwind-protect
        (progn
          (write-region "((not valid" nil (locate-user-emacs-file "yeetube"))
          (yeetube-load-saved-videos)
          (should (null yeetube-saved-videos)))
      (delete-directory temp-dir t))))

;;; Group 6: yeetube-mode-map keybindings

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

;;; Group 7: channel browsing

(ert-deftest yeetube-test-channel-videos-fetches-videos-tab ()
  "Channel videos fetches the channel videos page."
  (let (fetched-url)
    (cl-letf (((symbol-function 'yeetube--fetch)
               (lambda (request &rest _)
                 (setq fetched-url (plist-get request :url)))))
      (yeetube-channel-videos "/channel/UCsystemcrafters"))
    (should (equal "https://youtube.com/channel/UCsystemcrafters/videos"
                   fetched-url))))

(ert-deftest yeetube-test-channel-search-fetches-search-tab ()
  "Channel search fetches the channel search page."
  (let (fetched-url)
    (cl-letf (((symbol-function 'yeetube--fetch)
               (lambda (request &rest _)
                 (setq fetched-url (plist-get request :url)))))
      (yeetube-channel-search "@foo" "hello world"))
    (should (equal "https://youtube.com/@foo/search?query=hello%20world"
                   fetched-url))))

(ert-deftest yeetube-test-channel-search-errors-without-channel ()
  "Channel search requires a channel ID."
  (should-error (yeetube-channel-search nil "test") :type 'user-error)
  (should-error (yeetube-channel-search "  " "test") :type 'user-error))

(ert-deftest yeetube-test-display-channel-errors-without-channel ()
  "Channel browsing requires a channel ID."
  (should-error (yeetube--display-channel nil 'videos)
                :type 'user-error)
  (should-error (yeetube--display-channel "  " 'streams)
                :type 'user-error))

;;; Group 8: feed callback fallback

(defun yeetube-test--feed-callback-fallback-url (status fallback-url &optional contents)
  "Call `yeetube--feed-callback' with STATUS, FALLBACK-URL and CONTENTS."
  (let ((buffer (generate-new-buffer " *yeetube-feed-test*"))
        captured-url)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (when contents
              (insert contents))
            (cl-letf (((symbol-function 'yeetube-display-content-from-url)
                       (lambda (url)
                         (setq captured-url url))))
              (yeetube--feed-callback status fallback-url)))
          captured-url)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yeetube-test-feed-callback-falls-back-on-retrieval-error ()
  "Feed callback displays the fallback URL when retrieval fails."
  (should (equal "https://youtube.com/@systemcrafters/videos"
                 (yeetube-test--feed-callback-fallback-url
                  '(:error (error http 500))
                  "https://youtube.com/@systemcrafters/videos"))))

(ert-deftest yeetube-test-feed-callback-falls-back-on-malformed-xml ()
  "Feed callback displays the fallback URL when XML parsing fails."
  (should (equal "https://youtube.com/@systemcrafters/videos"
                 (yeetube-test--feed-callback-fallback-url
                  nil
                  "https://youtube.com/@systemcrafters/videos"
                  "<feed><entry>"))))

(ert-deftest yeetube-test-feed-callback-falls-back-on-empty-feed ()
  "Feed callback displays the fallback URL when the feed has no videos."
  (should (equal "https://youtube.com/@systemcrafters/videos"
                 (yeetube-test--feed-callback-fallback-url
                  nil
                  "https://youtube.com/@systemcrafters/videos"
                  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:yt=\"http://www.youtube.com/xml/schemas/2015\">
</feed>"))))

;;; Group 9: HTTP request headers

(ert-deftest yeetube-test-request-headers-cookie-bypasses-consent ()
  "Cookie header carries ucbcb / gdpr consent-bypass values."
  (let ((cookie (cdr (assoc "Cookie" yeetube-request-headers))))
    (should (stringp cookie))
    (should (string-match-p "ucbcb=1" cookie))
    (should (string-match-p "gdpr=1" cookie))
    (should (string-match-p "cookieconsent_status=allow" cookie))))

;;; Group 10: yeetube-mode buffer-local settings

(ert-deftest yeetube-test-mode-sets-truncate-string-ellipsis ()
  "yeetube-mode sets truncate-string-ellipsis to a single space."
  (let ((yeetube-content nil)
        (yeetube-display-thumbnails-p nil))
    (with-temp-buffer
      (yeetube-mode)
      (should (string= " " truncate-string-ellipsis)))))

(defun yeetube-test--call-response-callback (callback status display-text)
  "Call CALLBACK with STATUS while the yeetube buffer shows DISPLAY-TEXT.
Return a cons of the resulting display text and captured message."
  (let ((response-buffer (generate-new-buffer " *yeetube-response-test*"))
        captured-message)
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create yeetube--buffer-name)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert display-text)))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq captured-message
                             (apply #'format format-string args)))))
            (with-current-buffer response-buffer
              (funcall callback status)))
          (cons (with-current-buffer yeetube--buffer-name (buffer-string))
                captured-message))
      (when (buffer-live-p response-buffer)
        (kill-buffer response-buffer))
      (when (get-buffer yeetube--buffer-name)
        (kill-buffer yeetube--buffer-name)))))

(ert-deftest yeetube-test-page-callback-reports-http-error ()
  "An initial HTTP error clears the loading indicator and notifies the user."
  (pcase-let ((`(,display . ,message)
               (yeetube-test--call-response-callback
                #'yeetube--page-callback '(:error (error http 500)) "Loading...")))
    (should-not (string-match-p "Loading" display))
    (should (string-match-p "Could not retrieve backend response" message))))

(ert-deftest yeetube-test-continuation-callback-reports-http-error ()
  "A continuation HTTP error preserves results and notifies the user."
  (pcase-let ((`(,display . ,message)
               (yeetube-test--call-response-callback
                #'yeetube--continuation-callback
                '(:error (error http 500)) "Existing results")))
    (should (string= "Existing results" display))
    (should (string-match-p "Could not retrieve backend response" message))))

(ert-deftest yeetube-test-page-callback-reports-empty-results ()
  "A successful empty page replaces the loading indicator with a message."
  (cl-letf (((symbol-function 'yeetube--decode-url-buffer) #'ignore)
            ((symbol-function 'yeetube-backend-parse-page)
             (lambda (&rest _) '(:items nil))))
    (pcase-let ((`(,display . ,message)
                 (yeetube-test--call-response-callback
                  #'yeetube--page-callback nil "Loading...")))
      (should (string= "No videos found" display))
      (should (string= "No videos found" message)))))

(ert-deftest yeetube-test-replay-rejects-unknown-title ()
  "Replay never calls the player for an unknown title."
  (let ((yeetube-history '((:title "Known" :url "https://example.com/known")))
        played)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Unknown"))
              (yeetube-play-function (lambda (&rest args) (setq played args))))
      (should-error (yeetube-replay) :type 'user-error)
      (should-not played))))

(ert-deftest yeetube-test-download-requires-torsocks-when-tor-enabled ()
  "Tor-enabled downloads fail closed without torsocks."
  (let ((yeetube-ytdlp-program "yt-dlp")
        (yeetube-enable-tor t)
        (yeetube-torsocks-program nil)
        called)
    (cl-letf (((symbol-function 'call-process-shell-command)
               (lambda (&rest _) (setq called t))))
      (should-error (yeetube-download--ytdlp "https://example.com/video")
                    :type 'user-error)
      (should-not called))))

(ert-deftest yeetube-test-download-uses-configured-torsocks ()
  "Tor-enabled downloads invoke configured torsocks."
  (let ((yeetube-ytdlp-program "yt-dlp")
        (yeetube-enable-tor t)
        (yeetube-torsocks-program "torsocks")
        command)
    (cl-letf (((symbol-function 'call-process-shell-command)
               (lambda (value &rest _) (setq command value))))
      (yeetube-download--ytdlp "https://example.com/video")
      (should (string-prefix-p "torsocks yt-dlp " command)))))

(ert-deftest yeetube-test-video-switch-label ()
  "Video switch label describes the enabled playback state."
  (let ((yeetube-mpv-no-video nil))
    (should (string= "Video [on]" (yeetube--video-switch-label))))
  (let ((yeetube-mpv-no-video t))
    (should (string= "Video [off]" (yeetube--video-switch-label)))))

(provide 'yeetube-tests)
;;; yeetube-tests.el ends here
