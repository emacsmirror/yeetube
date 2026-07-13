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

;;; Format quality handling

(ert-deftest yeetube-mpv-test-nil-video-quality-uses-default ()
  "Nil video quality produces the valid default 720p format."
  (should (equal (yeetube-mpv-ytdl-format-video-quality nil)
                 (yeetube-mpv-ytdl-format-video-quality "720")))
  (should-not (string-match-p "nil"
                              (yeetube-mpv-ytdl-format-video-quality nil))))

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
