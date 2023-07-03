;;; yeetube.el --- Watch & Download Videos  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.com>
;; Keywords: extensions
;; URL: https://git.sr.ht/~thanosapollo/yeetube.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))

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

;; Search, play and downlaod videos from your desired video search
;; engine from Emacs.

;;; Code:

(require 'url)
(require 'org-element)
(require 'cl-lib)


(defgroup yeetube nil
  "Search, Play & Download videos."
  :group 'external
  :prefix "yeetube-")

(defcustom yeetube-results-limit 10
  "Define the amount of search results."
  :type 'number
  :safe #'numberp
  :group 'yeetube)

(defcustom yeetube-results-prefix "+"
  "Define prefix to display results with."
  :type 'string
  :safe #'stringp
  :group 'yeetube)

(defcustom yeetube-query-url "https://www.youtube.com/results?search_query="
  "Search URL."
  :type 'string
  :safe #'stringp
  :group 'yeetube)

(defcustom yeetube-download-audio-format nil
  "Select download video as audio FORMAT.
If nil download output will be the default format.

Example Usage:
 (setq yeetube-download-audio-format \"m4a\")"
  :type 'string
  :safe #'stringp
  :group 'yeetube)


(defcustom yeetube-player (executable-find "mpv")
  "Select default video player.

Example Usage:
 (setq yeetube-player \"mpv --no-video\")"
  :type 'string
  :safe #'stringp
  :group 'yeetube)

(defcustom yeetube-download-directory "~/Downloads"
  "Default directory to downlaod videos."
  :type 'string
  :safe #'stringp
  :group 'yeetube)

(define-minor-mode yeetube-mode
  "Yeetube mode."
  :init-value nil
  :lighter " yeetube-mode"
  :keymap (let ((yeetube-mode-map (make-sparse-keymap)))
	    (define-key yeetube-mode-map (kbd "RET") 'yeetube-play)
	    (define-key yeetube-mode-map (kbd "d") 'yeetube-download-video)
	    (define-key yeetube-mode-map (kbd "q") 'kill-current-buffer)
            yeetube-mode-map))

(defun yeetube-play ()
  "Open the url at point in an `'org-mode buffer using 'yeetube-player'."
  (interactive)
  (let ((url (org-element-property
	      :raw-link (org-element-context)))
	(buffer (get-buffer "*Yeetube Player*")))
    (shell-command (format "pkill -9 -f %s" (shell-quote-argument yeetube-player)))
    (when (string-prefix-p "http" url)
      (call-process-shell-command
       (format "%s %s" yeetube-player url) nil 0)
      (message "Opening %s" url)
      (switch-to-buffer buffer))))


;; TODO: Check if video_type of videoid is short or video
(defun yeetube-search (query)
  "Search for QUERY."
  (interactive "sYeetube Search: ")
  (let ((videoIds '())
        (videoTitles '()))
    (with-current-buffer (url-retrieve-synchronously (concat yeetube-query-url query) t t)
      (goto-char (point-min))
      (toggle-enable-multibyte-characters)
      (while (< (length videoIds) yeetube-results-limit)
	(search-forward "videoId")
        (let* ((start (point))
               (end (search-forward ","))
               (videoid (buffer-substring (+ start 3) (- end 2))))
          (unless (or (member videoid videoIds)
                      (not (and (>= (length videoid) 9)
                                (<= (length videoid) 13)
                                (string-match-p "^[a-zA-Z0-9_-]*$" videoid))))
            (push videoid videoIds)
            (search-forward "text")
            (let* ((start (point))
                   (end (search-forward ",\""))
                   (title (buffer-substring (+ start 3) (- end 5))))
              (push title videoTitles))))))
    (with-current-buffer (switch-to-buffer
                          (get-buffer-create "*Yeetube Search*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (org-mode)
      (insert
       "\n* Search Results: \n \n")
      (cl-loop for (videoId . videoTitle) in
	       (cl-mapcar #'cons (reverse videoIds) (reverse videoTitles))
               do (insert (format "%s [[https://www.youtube.com/watch?v=%s][%s ]]\n"
				  yeetube-results-prefix videoId videoTitle)))
      (insert
       "\n\n"
       "\n~RET~     -> Play Video\n"
       "\n~d~       -> Download\n"
       "\n~C-c C-o~ -> Open In Browser\n"
       "\n~q~       -> Quit\n")
      (setq buffer-read-only t)
      (goto-char (point-min))
      (search-forward yeetube-results-prefix)
      (yeetube-mode))))

(defun yeetube-download-video ()
  "Download using link at point in an `'org-mode buffer with yt-dlp."
  (interactive)
  (let ((url (org-element-property
	      :raw-link (org-element-context))))
    (when (string-prefix-p "http" url)
      (let ((default-directory yeetube-download-directory))
	(async-shell-command (format "yt-dlp %s" url))
	(message "Downloading %s " url)))))

(defun yeetube-download-videos ()
  "Download one or multiple videos using yt-dlp.

This command is not meant to be used through the
*Yeetube Search* buffer.

Usage Example:
Open a Dired buffer and navigate where you want to download your videos,
then run this command interactively."
  (interactive)
  (let ((links '())
        (names '())
        (url "")
        (name "")
        (buffer-counter 1)
        (name-counter 1))
    ;; Read links and names until "q" is entered
    (while (not (string= url "q"))
      (setq url (read-string "Enter URL (q to quit): "))
      (unless (string= url "q")
        (setq links (cons url links))
        (setq name (read-string (format "Enter name (%d-NAME): " name-counter)))
        (while (get-buffer (format "download-video-%d" buffer-counter))
          (setq buffer-counter (1+ buffer-counter)))
        (setq names (cons name names))
        (setq buffer-counter (1+ buffer-counter))
        (setq name-counter (1+ name-counter))))
    ;; Process the collected links and names
    (setq links (reverse links))
    (setq names (reverse names))
    (dolist (pair (cl-mapcar 'cons links names))
      (let ((url (car pair))
            (name (cdr pair))
            (buffer-name (format "download-video-%d" buffer-counter)))
        (async-shell-command (format "yt-dlp %s -o %s" url name) buffer-name)
        (setq buffer-counter (1+ buffer-counter))))))


(provide 'yeetube)
;;; yeetube.el ends here
