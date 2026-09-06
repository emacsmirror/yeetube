;;; yeetube-playback-tests.el --- Playback/settings regressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;;; Commentary:
;; Exercise command contracts and the mpv option boundary without media playback.

;;; Code:

(require 'ert)
(require 'yeetube)

(defmacro yeetube-playback--with-entry (type &rest body)
  "Run BODY at a real YeeTube row with item TYPE in a temporary buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (yeetube-mode)
     (setq-local yeetube-items (list (list :id "abc" :title "Title" :type ,type))
                 tabulated-list-format [("Title" 20 t)]
                 tabulated-list-entries '(("abc" ["Title"])))
     (tabulated-list-print)
     (goto-char (point-min))
     ,@body))

(ert-deftest yeetube-playback-custom-player-one-argument ()
  "All playback commands honor single-argument players with either modeline state."
  (dolist (modeline '(nil t))
    (yeetube-playback--with-entry 'video
      (let* ((yeetube-mpv-modeline-mode modeline)
             (url (yeetube-get-url "abc"))
             (yeetube-history (list (list :url url :title "Title")))
             (yeetube-saved-videos (list (cons "Title" url)))
             calls
             (yeetube-play-function (lambda (input) (push input calls))))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Title"))
                  ((symbol-function 'yeetube-load-saved-videos) #'ignore))
          (yeetube-play)
          (yeetube-replay)
          (yeetube-play-saved-video)
          (should (equal calls (list url url url))))))))

(ert-deftest yeetube-playback-mpv-modeline-title ()
  "Only the built-in player receives optional modeline titles."
  (dolist (modeline '(nil t))
    (yeetube-playback--with-entry 'video
      (let ((yeetube-mpv-modeline-mode modeline)
            (yeetube-play-function #'yeetube-mpv-play)
            (yeetube-history '((:url "https://example.invalid/v" :title "Title")))
            (yeetube-saved-videos '(("Title" . "https://example.invalid/v")))
            titles)
        (cl-letf (((symbol-function 'yeetube-mpv-play)
                   (lambda (_input &optional info) (push info titles)))
                  ((symbol-function 'completing-read) (lambda (&rest _) "Title"))
                  ((symbol-function 'yeetube-load-saved-videos) #'ignore))
          (yeetube-play)
          (yeetube-replay)
          (yeetube-play-saved-video)
          (should (equal titles (make-list 3 (and modeline "Title")))))))))

(ert-deftest yeetube-playback-audio-none-and-global-default ()
  "An explicit local none wins; outside result buffers use the global default."
  (let ((yeetube-download-audio-format "mp3")
        (yeetube-download-directory temporary-file-directory)
        calls)
    (cl-letf (((symbol-function 'yeetube-download--ytdlp)
               (lambda (&rest args) (push args calls))))
      (with-temp-buffer
        (yeetube-mode)
        (should (equal yeetube--audio-format "mp3"))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "none")))
          (yeetube-set-audio-format))
        (yeetube-download-video "https://example.invalid/v")
        (setq-local yeetube--audio-format "opus")
        (yeetube-download-video "https://example.invalid/v"))
      (with-temp-buffer
        (yeetube-download-video "https://example.invalid/v"))
      (should (equal (reverse calls)
                     '(("https://example.invalid/v" nil nil)
                       ("https://example.invalid/v" nil "opus")
                       ("https://example.invalid/v" nil "mp3")))))))

(ert-deftest yeetube-playback-save-infers-type-and-preserves-format ()
  "Save selected playlists by default and preserve prefix and old disk format."
  (let* ((directory (make-temp-file "yeetube-bookmarks-" t))
         (user-emacs-directory (file-name-as-directory directory))
         (yeetube-saved-videos nil))
    (unwind-protect
        (dolist (case '((playlist nil playlist) (video nil video) (video (4) playlist)))
          (yeetube-playback--with-entry (car case)
            (let ((expected (yeetube-get-url "abc" (nth 2 case))))
              (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Saved")))
                (yeetube-save-video (nth 1 case)))
              (should (equal (car yeetube-saved-videos) (cons "Saved" expected)))
              (setq yeetube-saved-videos nil)
              (yeetube-load-saved-videos)
              (should (equal (car yeetube-saved-videos) (cons "Saved" expected))))))
      (delete-directory directory t))))

(ert-deftest yeetube-playback-mpv-argv-and-option-boundary ()
  "Preserve special paths through the shell and mpv's real option parser."
  ;; No media is opened: the fake player only records argv.  If mpv is
  ;; available, a separate idle process reads its parsed option then quits.
  (skip-unless (executable-find "sh"))
  (let* ((directory (make-temp-file "yeetube-mpv-" t))
         (player (expand-file-name "fake mpv,'$\\player" directory))
         (capture (expand-file-name "argv" directory))
         (script (expand-file-name "probe.lua" directory))
         (parsed (expand-file-name "parsed" directory))
         (real-mpv (executable-find "mpv"))
         (process-environment (copy-sequence process-environment))
         (coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix)
         (yeetube-mpv-program player)
         (yeetube-mpv-enable-torsocks nil)
         (yeetube-mpv-video-quality "720")
         (yeetube-mpv-no-video t)
         (yeetube-mpv-additional-flags '("--title=My Title, '$\\"))
         (yeetube-mpv-currently-playing nil)
         (yeetube-mpv--process-name "yeetube-playback-test")
         proc)
    (unwind-protect
        (progn
          (setenv "YEETUBE_TEST_ARGV" capture)
          (setenv "YEETUBE_TEST_PARSED" parsed)
          (with-temp-file player
            (insert "#!/bin/sh\nprintf '%s\\0' \"$@\" > \"$YEETUBE_TEST_ARGV\"\n"))
          (set-file-modes player #o700)
          (with-temp-file script
            (insert "local o = {ytdl_path = ''}\n"
                    "require('mp.options').read_options(o, 'ytdl_hook')\n"
                    "local f = assert(io.open(os.getenv('YEETUBE_TEST_PARSED'), 'wb'))\n"
                    "f:write(o.ytdl_path); f:close()\nmp.commandv('quit')\n"))
          (with-temp-file (expand-file-name "--version" directory)
            (insert "Not media\n"))
          (dolist (case '(("/opt/custom yt-dlp" . "--version")
                          ("/opt/yt,dlp" . "https://example.invalid/v?q=one&x='two'")
                          ("/opt/é'$\\%=[]/yt-dlp" . "--script=local.lua")))
            (let* ((default-directory (file-name-as-directory directory))
                   (path (car case))
                   (yeetube-ytdlp-program path)
                   (input (cdr case))
                   (option (concat "--script-opts-append=ytdl_hook-ytdl_path=" path)))
              (setq proc (yeetube-mpv-play input "Title"))
              (let ((deadline (+ (float-time) 5)))
                (while (and (process-live-p proc) (< (float-time) deadline))
                  (accept-process-output proc 0.05)))
              (should-not (process-live-p proc))
              (should (= (process-exit-status proc) 0))
              (with-temp-buffer
                (insert-file-contents capture)
                (should (equal (split-string (buffer-string) "\0" t)
                               (list "--ytdl-format=bestvideo[height<=?720]+bestaudio/best"
                                     option "--no-video" "--title=My Title, '$\\"
                                     "--" input))))
              (when real-mpv
                (with-temp-buffer
                  (should (= 0 (call-process
                                real-mpv nil t nil "--no-config" "--load-scripts=no"
                                "--idle=yes" "--vo=null" "--ao=null"
                                (concat "--script=" script) option))))
                (with-temp-buffer
                  (insert-file-contents parsed)
                  (should (equal (buffer-string) path)))))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when-let* ((buffer (get-buffer "*yeetube-mpv-output*")))
        (kill-buffer buffer))
      (delete-directory directory t))))

(provide 'yeetube-playback-tests)
;;; yeetube-playback-tests.el ends here
