;;; yeetube-mpv-tests.el --- Tests for yeetube-mpv  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Run: emacs -Q --batch -L .. -l test/yeetube-mpv-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(unless (boundp 'find-function-mode)
  (defvar find-function-mode nil))

(require 'ert)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'yeetube-mpv)

(defvar yeetube-mpv-enable-torsocks)
(defvar yeetube-torsocks-program)
(defvar yeetube-ytdlp-program)
(defvar yeetube-mpv-video-quality)
(defvar yeetube-mpv-no-video nil)

;;; Format quality handling

(ert-deftest yeetube-mpv-test-nil-video-quality-uses-default ()
  "Nil video quality produces the valid default 720p format."
  (should (equal (yeetube-mpv-ytdl-format-video-quality nil)
                 (yeetube-mpv-ytdl-format-video-quality "720")))
  (should-not (string-match-p "nil"
                              (yeetube-mpv-ytdl-format-video-quality nil))))

(ert-deftest yeetube-mpv-test-torsocks-missing-program-errors ()
  "Enabled torsocks without a program signals a clear user error."
  (let ((yeetube-mpv-enable-torsocks t)
        (yeetube-torsocks-program nil)
        (yeetube-mpv-program "mpv")
        (yeetube-ytdlp-program "yt-dlp")
        (yeetube-mpv-video-quality "720")
        (yeetube-mpv-no-video nil)
        (yeetube-mpv-additional-flags nil)
        (process-started nil))
    (cl-letf (((symbol-function 'yeetube-mpv-check) #'ignore)
              ((symbol-function 'yeetube-mpv-process)
               (lambda (_command)
                 (setf process-started t))))
      (should-error (yeetube-mpv-play "https://example.com/video")
                    :type 'user-error)
      (should-not process-started))))

(ert-deftest yeetube-mpv-test-torsocks-program-is-used ()
  "Configured torsocks is prepended to the mpv command."
  (let ((yeetube-mpv-enable-torsocks t)
        (yeetube-torsocks-program "torsocks")
        (yeetube-mpv-program "mpv")
        (yeetube-ytdlp-program "yt-dlp")
        (yeetube-mpv-video-quality "720")
        (yeetube-mpv-no-video nil)
        (yeetube-mpv-additional-flags nil)
        command)
    (cl-letf (((symbol-function 'yeetube-mpv-check) #'ignore)
              ((symbol-function 'yeetube-mpv-process)
               (lambda (value)
                 (setf command value))))
      (yeetube-mpv-play "https://example.com/video")
      (should (string-prefix-p "torsocks mpv " command)))))

(ert-deftest yeetube-mpv-test-additional-flags-are-shell-quoted ()
  "Additional mpv flags with spaces stay single shell tokens."
  (let ((yeetube-mpv-enable-torsocks nil)
        (yeetube-mpv-program "mpv")
        (yeetube-ytdlp-program "yt-dlp")
        (yeetube-mpv-video-quality "720")
        (yeetube-mpv-no-video nil)
        (yeetube-mpv-additional-flags '("--title=My Title" "--foo bar"))
        command)
    (cl-letf (((symbol-function 'yeetube-mpv-check) #'ignore)
              ((symbol-function 'yeetube-mpv-process)
               (lambda (value)
                 (setf command value))))
      (yeetube-mpv-play "https://example.com/video")
      (should (string-match-p
               (regexp-quote (shell-quote-argument "--title=My Title"))
               command))
      (should (string-match-p
               (regexp-quote (shell-quote-argument "--foo bar"))
               command))
      (should-not (string-match-p " --title=My Title " command)))))

(ert-deftest yeetube-mpv-test-program-and-torsocks-are-shell-quoted ()
  "Spaced mpv and torsocks paths remain single shell tokens."
  (let ((yeetube-mpv-enable-torsocks t)
        (yeetube-torsocks-program "/opt/my torsocks/bin/torsocks")
        (yeetube-mpv-program "/opt/my mpv/bin/mpv")
        (yeetube-ytdlp-program "yt-dlp")
        (yeetube-mpv-video-quality "720")
        (yeetube-mpv-no-video nil)
        (yeetube-mpv-additional-flags nil)
        command)
    (cl-letf (((symbol-function 'yeetube-mpv-check) #'ignore)
              ((symbol-function 'yeetube-mpv-process)
               (lambda (value)
                 (setf command value))))
      (yeetube-mpv-play "https://example.com/video")
      (should (string-prefix-p
               (concat (shell-quote-argument yeetube-torsocks-program) " "
                       (shell-quote-argument yeetube-mpv-program) " ")
               command))
      (should-not (string-match-p "^/opt/my torsocks/" command)))))

(ert-deftest yeetube-mpv-test-hook-separator-rejected-before-process ()
  "Reject executable paths the hook would split before replacing a player."
  (skip-unless (not (eq system-type 'windows-nt)))
  (let* ((directory (make-temp-file "yeetube-hook-path-" t))
         (prefix (expand-file-name "downloader" directory))
         (yeetube-ytdlp-program (concat prefix ":custom"))
         (yeetube-mpv-program "mpv")
         (yeetube-mpv-enable-torsocks nil)
         (yeetube-mpv-video-quality "720")
         (yeetube-mpv-no-video nil)
         (yeetube-mpv-additional-flags nil)
         (yeetube-mpv-currently-playing "[Existing player]")
         process-called)
    (unwind-protect
        (progn
          ;; Both names are legitimate executables.  Quoting only the outer
          ;; option would let ytdl_hook execute PREFIX instead of :custom.
          (dolist (path (list prefix yeetube-ytdlp-program))
            (with-temp-file path (insert "#!/bin/sh\nexit 0\n"))
            (set-file-modes path #o700)
            (should (file-executable-p path)))
          (cl-letf (((symbol-function 'yeetube-mpv-process)
                     (lambda (_command) (setq process-called t))))
            (let ((err (should-error (yeetube-mpv-play "offline://probe" "New")
                                     :type 'user-error)))
              (should (string-match-p "yeetube-ytdlp-program"
                                      (error-message-string err)))
              (should (string-match-p "symlink" (error-message-string err)))))
          ;; This is the boundary that deletes the old process and launches
          ;; mpv: neither effect may occur, even for an existing executable.
          (should-not process-called)
          (should (equal yeetube-mpv-currently-playing "[Existing player]")))
      (delete-directory directory t))))

(ert-deftest yeetube-mpv-test-hook-separator-is-platform-specific ()
  "Use the native hook separator, not a drive colon on Windows."
  (dolist (case '((gnu/linux "/opt/yt:dlp" t)
                  (darwin "/opt/yt:dlp" t)
                  (gnu/linux "/opt/yt;dlp" nil)
                  (windows-nt "C:/Tools/yt;dlp.exe" t)
                  (windows-nt "C:/Tools/yt-dlp.exe" nil)))
    (let ((system-type (nth 0 case))
          (yeetube-ytdlp-program (nth 1 case))
          (yeetube-mpv-program "mpv")
          (yeetube-mpv-enable-torsocks nil)
          (yeetube-mpv-video-quality "720")
          (yeetube-mpv-no-video nil)
          (yeetube-mpv-additional-flags nil)
          (yeetube-mpv-currently-playing nil)
          (host-system-type (default-value 'system-type))
          (quote-argument (symbol-function 'shell-quote-argument))
          process-called)
      ;; Only the hook policy is simulated; use host shell quoting because
      ;; a Unix Emacs does not implement Windows shell primitives.
      (cl-letf (((symbol-function 'shell-quote-argument)
                 (lambda (argument)
                   (let ((system-type host-system-type))
                     (funcall quote-argument argument))))
                ((symbol-function 'yeetube-mpv-process)
                 (lambda (_command) (setq process-called t))))
        (if (nth 2 case)
            (progn
              (should-error (yeetube-mpv-play "offline://probe") :type 'user-error)
              (should-not process-called))
          (yeetube-mpv-play "offline://probe")
          (should process-called))))))

;;; Process sentinel: clears modeline state on exit

(ert-deftest yeetube-mpv-test-sentinel-clears-on-exit ()
  "Modeline state is cleared when the mpv process exits normally."
  (cl-letf (((symbol-function 'yeetube-mpv-check) #'ignore))
    (let ((yeetube-mpv-currently-playing "[Stale Title]"))
      (let ((proc (yeetube-mpv-process "true")))
        (unwind-protect
            (progn
              (should (processp proc))
              (while (process-live-p proc)
                (accept-process-output nil 0.1))
              (accept-process-output nil 0.1)
              (should (null yeetube-mpv-currently-playing)))
          (when (processp proc)
            (delete-process proc)))))))

(ert-deftest yeetube-mpv-test-sentinel-clears-on-signal ()
  "Modeline state is cleared when the mpv process is killed."
  (cl-letf (((symbol-function 'yeetube-mpv-check) #'ignore))
    (let ((yeetube-mpv-currently-playing "[Stale Title]"))
      (let ((proc (yeetube-mpv-process "sleep 30")))
        (unwind-protect
            (progn
              (should (processp proc))
              (kill-process proc)
              (while (process-live-p proc)
                (accept-process-output nil 0.1))
              (accept-process-output nil 0.1)
              (should (null yeetube-mpv-currently-playing)))
          (when (processp proc)
            (delete-process proc)))))))

(provide 'yeetube-mpv-tests)
;;; yeetube-mpv-tests.el ends here
