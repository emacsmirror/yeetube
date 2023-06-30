;;; yeetube.el --- Watch & Download Videos  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.com>
;; Keywords: extensions

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

;; 

;;; Code:

(require 'url)
(require 'org-element)


(defcustom yt-search-query "https://www.youtube.com/results?search_query="
  "Search URL."
  :type 'string
  :group 'youtube)

(defcustom yt-download-audio-format nil
  "Select download video as audio FORMAT.
If nil yt-download-videos output will be the default format.

Example Usage:
 (setq yt-download-audio-format \"m4a\")"
  :type 'string
  :group 'youtube)


;; TODO: Make a defcustom for video player
(defun yt-play ()
  "Open the link at point in an `'org-mode buffer with `'mpv."
  (interactive)
  (let ((url (org-element-property
	      :raw-link (org-element-context))))
    (when (string-prefix-p "http" url)
      (async-shell-command (format "mpv %s" url))
      (message "Opening %s with mpv" url))))



;; TODO Break this function into smaller ones, repeat search for unique videoIds
(defun yt-search (arg)
  "Search for ARG in YouTube."
  (interactive "sYoutube Search: ")
  (let ((videoIds '())
	(videoTitles '()))
    (with-current-buffer (url-retrieve-synchronously (concat yt-search-query arg))
      (goto-char (point-min))
	(search-forward "videoId")
	(let* ((start (point))
	       (end (search-forward ","))
	       (videoid (buffer-substring (+ start 3) (- end 2))))
	  (if (not (member videoid videoIds))
	      (push videoid videoIds)))
	(search-forward "text")
	(let* ((start (point))
	       (end (search-forward ","))
	       (title (buffer-substring (+ start 3) (- end 4))))
	  (if (not (member title videoTitles))
	      (push title videoTitles)))
      (with-current-buffer (switch-to-buffer
			    (get-buffer-create "*Youtube Search*"))
	(setq buffer-read-only nil)
	(erase-buffer)
	(org-mode)
	(dolist (videoId videoIds)
	  (dolist (videoTitle videoTitles)
	    (insert (format "- [[https://www.youtube.com/watch?v=%s][%s]] \n"
			    videoId
			    videoTitle))))
	(setq buffer-read-only t)))))


;; TODO: let user decide custom name and path
(defun yt-download-video ()
  "Download using link at point in an `'org-mode buffer with yt-dlp."
  (interactive)
  (let ((url (org-element-property
	      :raw-link (org-element-context))))
    (when (string-prefix-p "http" url)
      (async-shell-command (format "yt-dlp %s" url))
      (message "Downloading %s " url))))

(defun yt-download-videos ()
  "Download one or multiple videos using yt-dlp.

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