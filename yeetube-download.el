;;; yeetube-download.el --- YeeTube download support  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos
;; URL: https://thanosapollo.org/projects/yeetube/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Download support for YeeTube using yt-dlp.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
;; Forward declarations for variables defined in yeetube.el
(defvar yeetube-ytdlp-program)
(defvar yeetube-torsocks-program)
(defvar yeetube-enable-tor)
(defvar yeetube-download-directory)
(defvar yeetube-download-audio-format)

(defun yeetube-download--sentinel (process _event)
  "Report the terminal status of download PROCESS once.
Ignore the event text _EVENT; use the actual process status instead."
  (when (and (memq (process-status process) '(exit signal failed))
             (not (process-get process 'yeetube-download-reported)))
    (process-put process 'yeetube-download-reported t)
    (let* ((buffer (process-buffer process))
           (status (pcase (process-status process)
                     ('exit (if (zerop (process-exit-status process))
                                "Download finished successfully"
                              (format "Download failed (exit %d)"
                                      (process-exit-status process))))
                     ('signal (format "Download cancelled or terminated (signal %d)"
                                      (process-exit-status process)))
                     ('failed "Download failed to start"))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-max))
                (insert "\n" status "\n"))))))
      (message "%s%s" status
               (if (buffer-live-p buffer)
                   (format "; see %s" (buffer-name buffer))
                 "")))))

(defun yeetube-download--executable (program label)
  "Resolve executable PROGRAM or signal a user error naming LABEL."
  (or (and (stringp program) (not (string-empty-p program))
           (executable-find program))
      (user-error "Executable for %s not found" label)))

(defun yeetube-download--ytdlp (url &optional name audio-format)
  "Start downloading URL using yt-dlp and return its process.
Use optional NAME as the output filename, unless empty.  Non-nil
AUDIO-FORMAT extracts audio in that format; nil leaves video intact.
Download in `default-directory'.  Arguments are passed without a shell.
Keep output and final status in a unique *yeetube-download* buffer.
Kill that buffer (confirming process termination) or use `delete-process'
to cancel.  Signal an error if the executable is missing or launch fails."
  (let* ((torsocks (when yeetube-enable-tor
                     (yeetube-download--executable yeetube-torsocks-program
                                                   "torsocks")))
         (program (yeetube-download--executable yeetube-ytdlp-program "yt-dlp"))
         (command (append (when torsocks (list torsocks)) (list program)
                          (when (and name (not (string-empty-p name)))
                            (list "-o" name))
                          (when audio-format
                            (list "--extract-audio" "--audio-format" audio-format))
                          (list "--" url)))
         (buffer (generate-new-buffer "*yeetube-download*")))
    (condition-case err
        (progn
          (with-current-buffer buffer (special-mode))
          (let ((process (make-process :name "yeetube-download"
                                       :buffer buffer :command command
                                       :connection-type 'pipe
                                       :sentinel #'yeetube-download--sentinel)))
            (message "Download started; see %s" (buffer-name buffer))
            process))
      ((error quit)
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (signal (car err) (cdr err))))))

;;;###autoload
(defun yeetube-download-change-directory ()
  "Change download directory."
  (interactive)
  (setf yeetube-download-directory
        (read-directory-name "Select a directory: ")))

;;;###autoload
(defun yeetube-download-change-audio-format (audio-format)
  "Change download format to AUDIO-FORMAT."
  (interactive "sSpecify Audio Format(no for nil): ")
  (setf yeetube-download-audio-format (and (not (equal audio-format "no"))
                                           audio-format)))

;; TODO: Add option to use ffmpeg
;;;###autoload
(defun yeetube-download-videos ()
  "Bulk download videos using yt-dlp.
This command is not meant to be used in the *Yeetube Search* buffer.

Usage Example:
Open a Dired buffer and navigate where you want to download your
videos, then run this command interactively.  You can leave the name
prompt blank to keep the default name."
  (interactive)
  (let ((download-counter 1))
    (cl-loop
     for url = (read-string "Enter URL (q to quit): ")
     until (string= url "q")
     do (let ((name (read-string (format "Custom name (download counter: %d) "
					 download-counter))))
          (yeetube-download--ytdlp url name yeetube-download-audio-format)
          (cl-incf download-counter)))))

(provide 'yeetube-download)
;;; yeetube-download.el ends here
