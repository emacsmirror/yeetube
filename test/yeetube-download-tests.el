;;; yeetube-download-tests.el --- Download process tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise direct arguments and process settlement with disposable programs.
;; No network access or real downloads are used.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'yeetube-download)

(defvar yeetube-ytdlp-program)
(defvar yeetube-torsocks-program)
(defvar yeetube-enable-tor)
(defvar yeetube-download-audio-format)

(defun yeetube-download-test--script (directory name body)
  "Write an executable shell script named NAME in DIRECTORY with BODY."
  (let ((file (expand-file-name name directory)))
    (with-temp-file file (insert "#!/bin/sh\n" body "\n"))
    (set-file-modes file #o700)
    file))

(defmacro yeetube-download-test--with-directory (&rest body)
  "Run BODY with disposable executables and clean up its process."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "yeetube-download-test-" t))
          (default-directory (file-name-as-directory directory))
          (yeetube-enable-tor nil)
          (yeetube-torsocks-program nil)
          (yeetube-download-audio-format nil)
          (yeetube-ytdlp-program
           (yeetube-download-test--script directory "fake yt-dlp;safe"
                                         "printf '<%s>\\n' \"$@\""))
          (process nil))
     ;; These fixtures require a POSIX shell; no downloader is required.
     (skip-unless (file-executable-p "/bin/sh"))
     (unwind-protect
         (progn ,@body)
       (when (processp process)
         (when (process-live-p process) (delete-process process))
         (when (buffer-live-p (process-buffer process))
           (kill-buffer (process-buffer process))))
       (delete-directory directory t))))

(defun yeetube-download-test--wait (process &optional output)
  "Wait boundedly for PROCESS to settle or emit OUTPUT."
  (let ((deadline (+ (float-time) 5)))
    (while (and (< (float-time) deadline)
                (if output
                    (not (string-match-p
                          (regexp-quote output)
                          (with-current-buffer (process-buffer process)
                            (buffer-string))))
                  (not (process-get process 'yeetube-download-reported))))
      (accept-process-output process 0.05))
    (if output
        (should (string-match-p
                 (regexp-quote output)
                 (with-current-buffer (process-buffer process) (buffer-string))))
      (should (process-get process 'yeetube-download-reported)))))

(ert-deftest yeetube-download-test-direct-argv ()
  "Preserve spaces, shell syntax and URL option boundaries literally."
  (yeetube-download-test--with-directory
    (let ((url "--url ; $(touch INJECTED) & value")
          (name "name ' ; $(touch INJECTED) .mp4")
          (audio "mp3 ; $(touch INJECTED)"))
      (setq process (yeetube-download--ytdlp url name audio))
      (should (equal (cdr (process-command process))
                     (list "-o" name "--extract-audio" "--audio-format"
                           audio "--" url)))
      (yeetube-download-test--wait process)
      (should (equal (process-status process) 'exit))
      (should (zerop (process-exit-status process)))
      (with-current-buffer (process-buffer process)
        (should (string-prefix-p
                 (mapconcat (lambda (arg) (format "<%s>\n" arg))
                            (cdr (process-command process)) "")
                 (buffer-string)))
        (should (string-match-p "Download finished successfully" (buffer-string))))
      (should-not (file-exists-p (expand-file-name "INJECTED" directory))))))

(ert-deftest yeetube-download-test-default-directory-and-output ()
  "Use the caller's directory and capture both output streams."
  (yeetube-download-test--with-directory
    (setq yeetube-ytdlp-program
          (yeetube-download-test--script directory "output"
                                        "pwd; printf 'stderr-data\\n' >&2"))
    (setq process (yeetube-download--ytdlp "test"))
    (yeetube-download-test--wait process)
    (with-current-buffer (process-buffer process)
      (should (string-prefix-p (concat directory "\n") (buffer-string)))
      (should (string-match-p "stderr-data" (buffer-string))))))

(ert-deftest yeetube-download-test-blank-name-and-nil-audio ()
  "Omit output and extraction options when not requested."
  (dolist (name '(nil ""))
    (yeetube-download-test--with-directory
      (setq process (yeetube-download--ytdlp "test" name nil))
      (yeetube-download-test--wait process)
      (should (equal (cdr (process-command process)) '("--" "test"))))))

(ert-deftest yeetube-download-test-torsocks ()
  "Execute the configured wrapper with the downloader as one argument."
  (yeetube-download-test--with-directory
    (setq yeetube-enable-tor t
          yeetube-torsocks-program
          (yeetube-download-test--script
           directory "fake torsocks;safe" "printf 'wrapped\\n'; exec \"$@\""))
    (setq process (yeetube-download--ytdlp "test url"))
    (yeetube-download-test--wait process)
    (should (equal (process-command process)
                   (list yeetube-torsocks-program yeetube-ytdlp-program
                         "--" "test url")))
    (with-current-buffer (process-buffer process)
      (should (string-prefix-p "wrapped\n<-->\n<test url>\n" (buffer-string))))))

(ert-deftest yeetube-download-test-missing-executables ()
  "Reject absent downloaders and fail closed without torsocks."
  (yeetube-download-test--with-directory
    (dolist (missing (list nil "" (expand-file-name "missing" directory)))
      (let ((yeetube-ytdlp-program missing))
        (should-error (yeetube-download--ytdlp "test") :type 'user-error))
      (let ((yeetube-enable-tor t)
            (yeetube-torsocks-program missing))
        (should-error (yeetube-download--ytdlp "test") :type 'user-error)))))

(ert-deftest yeetube-download-test-launch-error-cleans-buffer ()
  "Preserve launch errors and remove the unused output buffer."
  (yeetube-download-test--with-directory
    (let (output)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq output (plist-get args :buffer))
                   (signal 'file-error '("Launch rejected")))))
        (should-error (yeetube-download--ytdlp "test") :type 'file-error))
      (should (bufferp output))
      (should-not (buffer-live-p output)))))

(ert-deftest yeetube-download-test-invalid-interpreter ()
  "An executable with a missing interpreter never reports success."
  (yeetube-download-test--with-directory
    (with-temp-file yeetube-ytdlp-program
      (insert "#!" (expand-file-name "missing-interpreter" directory) "\n"))
    ;; Emacs may reject exec synchronously or report its child exiting 127.
    (condition-case nil
        (progn
          (setq process (yeetube-download--ytdlp "test"))
          (yeetube-download-test--wait process)
          (should-not (zerop (process-exit-status process)))
          (with-current-buffer (process-buffer process)
            (should (string-match-p "Download failed" (buffer-string)))
            (should-not (string-match-p "successfully" (buffer-string)))))
      (file-error (should-not process)))))

(ert-deftest yeetube-download-test-nonzero ()
  "Report nonzero exit truthfully and retain diagnostic output."
  (yeetube-download-test--with-directory
    (setq yeetube-ytdlp-program
          (yeetube-download-test--script directory "failure"
                                        "printf 'diagnostic\\n' >&2; exit 7"))
    (setq process (yeetube-download--ytdlp "test"))
    (yeetube-download-test--wait process)
    (should (= 7 (process-exit-status process)))
    (with-current-buffer (process-buffer process)
      (let ((text (buffer-string)))
        (should (string-match-p "diagnostic" text))
        (should (string-match-p "Download failed (exit 7)" text))
        (should-not (string-match-p "successfully" text))
        (yeetube-download--sentinel process "finished\n")
        (should (equal text (buffer-string)))))))

(ert-deftest yeetube-download-test-cancel ()
  "Cancel a running process without announcing success."
  (yeetube-download-test--with-directory
    (setq yeetube-ytdlp-program
          (yeetube-download-test--script directory "waiting"
                                        "printf 'ready\\n'; read answer"))
    (setq process (yeetube-download--ytdlp "test"))
    (yeetube-download-test--wait process "ready")
    (delete-process process)
    (yeetube-download-test--wait process)
    (should (eq (process-status process) 'signal))
    (with-current-buffer (process-buffer process)
      (should (string-match-p "Download cancelled or terminated" (buffer-string)))
      (should-not (string-match-p "successfully" (buffer-string))))))

(ert-deftest yeetube-download-test-signal ()
  "Report a signal independently of explicit user cancellation."
  (yeetube-download-test--with-directory
    (setq yeetube-ytdlp-program
          (yeetube-download-test--script directory "signal" "kill -TERM $$"))
    (setq process (yeetube-download--ytdlp "test"))
    (yeetube-download-test--wait process)
    (should (eq (process-status process) 'signal))
    (with-current-buffer (process-buffer process)
      (should (string-match-p "Download cancelled or terminated" (buffer-string)))
      (should-not (string-match-p "successfully" (buffer-string))))))

(ert-deftest yeetube-download-test-buffer-teardown ()
  "Killing an output buffer cancels its process without recreating it."
  (yeetube-download-test--with-directory
    (setq yeetube-ytdlp-program
          (yeetube-download-test--script directory "waiting"
                                        "printf 'ready\\n'; read answer"))
    (setq process (yeetube-download--ytdlp "test"))
    (yeetube-download-test--wait process "ready")
    (let ((buffer (process-buffer process)))
      (set-process-query-on-exit-flag process nil)
      (kill-buffer buffer)
      (yeetube-download-test--wait process)
      (should-not (process-live-p process))
      (should-not (buffer-live-p buffer))
      (yeetube-download--sentinel process "finished\n")
      (should-not (buffer-live-p buffer)))))

(ert-deftest yeetube-download-test-standalone-bulk ()
  "Bulk input launches with its selected format without loading core."
  (yeetube-download-test--with-directory
    (let ((answers '("test url" "custom name" "q"))
          (yeetube-download-audio-format "opus")
          (start (symbol-function 'yeetube-download--ytdlp)))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (pop answers)))
                ((symbol-function 'yeetube-download--ytdlp)
                 (lambda (&rest args) (setq process (apply start args)))))
        (yeetube-download-videos))
      (yeetube-download-test--wait process)
      (should (equal (cdr (process-command process))
                     '("-o" "custom name" "--extract-audio" "--audio-format"
                       "opus" "--" "test url")))
      (should-not answers))))

(provide 'yeetube-download-tests)
;;; yeetube-download-tests.el ends here
