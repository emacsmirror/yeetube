;;; yeetube.el --- YouTube Front End  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos
;; URL: https://git.thanosapollo.org/yeetube
;; Version: 2.0.7


;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2"))

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

;; This package provides the ability to scrape YouTube, with the results
;; displayed in a proced-like buffer.
;;
;; Basic functionality includes:
;;
;; - Query YouTube
;; - Play video url by default using mpv
;; - Bookmark/Save video url
;; - Download video using yt-dlp
;; - A minimal yt-dlp front-end, which is independent of the rest YouTube functionality, to download multiple urls.

;;; Code:

(require 'compat)
(require 'url)
(require 'cl-lib)
(require 'yeetube-buffer)
(require 'yeetube-mpv)

(defgroup yeetube nil
  "Youtube Front-end."
  :group 'external
  :prefix "yeetube-")

(defcustom yeetube-results-limit 20
  "Define a limit for search results."
  :type 'natnump
  :group 'yeetube)

(defcustom yeetube-player #'yeetube-mpv-play
  "Select media player function."
  :type 'function
  :group 'yeetube)

(defcustom yeetube-download-audio-format nil
  "Select download video as audio FORMAT.
If nil download output will be the default format.

Example Usage:
 (setf yeetube-download-audio-format \"m4a\")"
  :type 'string
  :group 'yeetube)

(defcustom yeetube-download-directory "~/Downloads"
  "Default directory to downlaod videos."
  :type 'string
  :group 'yeetube)

(defvar yeetube-invidious-instances
  '("vid.puffyan.us"
    "invidious.flokinet.to"
    "yt.artemislena.eu"
    "invidious.privacydev.net"
    "onion.tube"
    "yewtu.be")
  "List of invidious instaces.")

(defvar yeetube-content nil
  "Scraped content.")

(defvar yeetube-saved-videos nil
  "Saved/bookmarked video urls.")

(defvar yeetube-history nil
  "Stored urls & titles of recently played content.")

(defvar yeetube-url "https://youtube.com/watch?v="
  "URL used to play videos from.

You can change the value to an invidious instance.")

(defun yeetube-get (keyword)
  "Retrieve KEYWORD value for entry at point.

Retrieve keyword value for entry at point, from `yeetube-content', in
*yeetube* buffer.

Keywords:
- :title
- :videoid
- :view-count
- :duration
- :channel"
  (unless (keywordp keyword)
    (error "Value `%s' is not a keyword" keyword))
  (let ((video-info
	 (cl-getf (nth (- (line-number-at-pos) 1) (reverse yeetube-content)) keyword)))
    video-info))

(defun yeetube-get-url ()
  "Get video url."
  (let ((video-url (concat yeetube-url (yeetube-get :videoid))))
    video-url))

;;;###autoload
(defun yeetube-play ()
  "Play video at point in *yeetube* buffer."
  (interactive)
  (let ((video-url (yeetube-get-url))
	(video-title (yeetube-get :title)))
    (funcall yeetube-player video-url)
    (push (list :url video-url :title video-title) yeetube-history)
    (message "Playing: %s" video-title)))

;;;###autoload
(defun yeetube-replay ()
  "Select entry from history to replay.

Select entry title from yeetube-history and play corresponding URL."
  (interactive)
  (let* ((titles (mapcar (lambda (entry) (cl-getf entry :title)) yeetube-history))
         (selected (completing-read "Replay: " titles))
         (selected-entry (cl-find-if (lambda (entry) (string= selected (cl-getf entry :title))) yeetube-history))
         (url (cl-getf selected-entry :url)))
    (funcall yeetube-player url)
    (message "Replaying: %s" selected)))

(defun yeetube-load-saved-videos ()
  "Load saved videos."
  (let ((file-path (concat user-emacs-directory "yeetube")))
    (if (file-exists-p file-path)
	(with-temp-buffer
	  (insert-file-contents file-path)
	  (goto-char (point-min))
	  (let ((contents (read (current-buffer))))
	    (setf yeetube-saved-videos contents)))
      (write-region "nil" nil file-path))))

;;;###autoload
(defun yeetube-save-video ()
  "Save url at point."
  (interactive)
  (yeetube-load-saved-videos)
  (let ((name (read-string "Save as: "))
	(url (yeetube-get-url)))
    (push (cons name url) yeetube-saved-videos)))

;; We could use keywords here, but it would break users saved videos
;; from previous versions.
;;;###autoload
(defun yeetube-play-saved-video ()
  "Select & Play a saved video."
  (interactive)
  (yeetube-load-saved-videos)
  (let ((video (completing-read "Select video: " yeetube-saved-videos nil t)))
    (funcall yeetube-player (cdr (assoc video yeetube-saved-videos)))
    (message "Playing: %s" (car (assoc video yeetube-saved-videos)))))

;;;###autoload
(defun yeetube-remove-saved-video ()
  "Select video to remove from saved videos."
  (interactive)
  (yeetube-load-saved-videos)
  (let ((video (completing-read "Select video: " yeetube-saved-videos nil t)))
    (setf yeetube-saved-videos (remove (assoc video yeetube-saved-videos) yeetube-saved-videos))))

;;;###autoload
(defun yeetube-remove-all-saved-videos ()
  "Clear yeetube saved."
  (interactive)
  (let ((clear-saved (y-or-n-p "Delete saved?")))
    (when clear-saved
      (setf yeetube-saved-videos nil))))

(defun yeetube-update-saved-videos-list (_symbol new-value _where _environment)
  "Updated saved videos.

SYMBOL-NAME is the name of the symbol to update.
NEW-VALUE is the new value for the symbol.
OPERATION is the operation to perform.
WHERE indicates where in the buffer the update should happen."
  (with-temp-buffer (find-file (concat user-emacs-directory "yeetube"))
		    (erase-buffer)
		    (setf yeetube-saved-videos new-value)
		    (insert (pp-to-string yeetube-saved-videos))
		    (save-buffer)
		    (kill-buffer)))

;;;###autoload
(defun yeetube-search (query)
  "Search for QUERY."
  (interactive "sYeetube Search: ")
  (with-current-buffer
      (url-retrieve-synchronously
       (concat "https://youtube.com/search?q="
	       (replace-regexp-in-string " " "+" query)
	       ;; Filter parameter to remove live videos.
	       "&sp=EgQQASAB")
       'silent 'inhibit-cookies 30)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (goto-char (point-min))
    (toggle-enable-multibyte-characters)
    (yeetube-get-content)
    (yeetube-buffer-create query yeetube-content 'yeetube-mode)))

;;;###autoload
(defun yeetube-browse-url ()
  "Open URL for video at point, using an invidious instance."
  (interactive)
  (let ((invidious-instance (+ 1 (random (length yeetube-invidious-instances)))))
    (browse-url
     (replace-regexp-in-string "youtube.com"
			       (nth invidious-instance yeetube-invidious-instances)
			       (yeetube-get-url)))))

(defun yeetube-get-item (query)
  "Get item from youtube results for QUERY.

Video result starts with videorenderer.
Search back to videorenderer (start of video results),
then for item."
  (search-backward "videorenderer" nil t)
  (search-forward query nil t)
  (search-forward "text" nil t))

(defvar yeetube--title-replacements
  '(("&amp;" . "&")
    ("&quot;" . "\"")
    ("&#39;" . "'")
    ("u0026" . "&")
    ("\\\\" . ""))
  "Unicode character replacements.")

;; Usually titles from youtube get messed up,
;; This should fix some of the common issues.
(defun yeetube---fix-title (title)
  "Adjust TITLE."
  (mapc (lambda (replacement)
          (setf title (replace-regexp-in-string (car replacement) (cdr replacement) title)))
        yeetube--title-replacements)
  (if yeetube-buffer-display-emojis
      title
    (yeetube-buffer-strip-emojis title)))

(defun yeetube-get-content ()
  "Get content from youtube."
  (setf yeetube-content nil)
  (while (and (< (length yeetube-content) yeetube-results-limit)
	      (search-forward "videorenderer" nil t))
    (search-forward "videoid")
    (let ((videoid (buffer-substring (+ (point) 3)
				     (- (search-forward ",") 2))))
      (unless (member videoid (car yeetube-content))
	(yeetube-get-item "title") ;; Video Title
        (let ((title (yeetube---fix-title
		      (buffer-substring (+ (point) 3)
					(- (search-forward ",\"") 5)))))
	  (unless (member title (car yeetube-content))
	    (yeetube-get-item "viewcounttext") ;; View Count
	    (let ((view-count (buffer-substring (+ (point) 3)
						(- (search-forward " ") 0))))
	      (yeetube-get-item "lengthtext") ;; Video Duration
	      (let ((video-duration (buffer-substring (+ (point) 3)
						      (- (search-forward "},") 3))))
		(yeetube-get-item "longbylinetext") ;; Channel Name
		(let ((channel (buffer-substring (+ (point) 3)
						 (- (search-forward ",") 2))))
		  (push (list :title title
			      :videoid videoid
			      :view-count view-count
			      :duration video-duration
			      :channel channel)
			yeetube-content))))))))))

(add-variable-watcher 'yeetube-saved-videos #'yeetube-update-saved-videos-list)

;; Yeetube Downlaod:

(defvar yeetube-ytdlp (executable-find "yt-dlp")
  "Path for yt-dlp executable.")

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
  (setf yeetube-download-audio-format audio-format)
  (when (equal yeetube-download-audio-format "no")
    (setf yeetube-download-audio-format nil)))

(defun yeetube-download--ytdlp (url &optional name audio-format)
  "Download URL using yt-dlp.

Optional values:
 NAME for custom file name.
 AUDIO-FORMAT to extract and keep contents as specified audio-format only."
  (unless yeetube-ytdlp
    (error "Executable for yt-dlp not found.  Please install yt-dlp"))
  (call-process-shell-command
   (concat "yt-dlp " (shell-quote-argument url)
	   (when name
	     " -o "(shell-quote-argument name))
	   (when audio-format
	     " --extract-audio --audio-format " (shell-quote-argument audio-format)))
   nil 0))

(defun yeetube-download--ffmpeg (url name)
  "Download URL as NAME using ffmpeg."
  (let ((ffmpeg (executable-find "ffmpeg")))
    (unless ffmpeg
      (error "Executable ffmpeg not found.  Please install ffmpeg"))
    (call-process-shell-command
     (concat ffmpeg
	     " -protocol_whitelist file,crypto,data,https,tls,tcp -stats -i "
	     (shell-quote-argument url)
	     " -codec copy "
	     name))))

;;;###autoload
(defun yeetube-download-video ()
  "Download entry at point in *yeetube* buffer with yt-dlp."
  (interactive)
  (let ((url (yeetube-get-url)))
    (when (string-prefix-p "http" url)
      (let ((default-directory yeetube-download-directory))
        (yeetube-download--ytdlp url nil yeetube-download-audio-format)
        (message "Downloading: '%s' at '%s'"
		 (yeetube-get :title) yeetube-download-directory)))))

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
  (let ((url "")
	(name "")
	(download-counter 1))
    (while (not (string= url "q"))
      (setf url (read-string "Enter URL (q to quit): "))
      (unless (string= url "q")
	(setf name (read-string (format "Custom name (download counter: %d) " download-counter)))
	(setf download-counter (1+ download-counter))
	(yeetube-download--ytdlp url name yeetube-download-audio-format)))))

;; Yeetube Mode

(defvar-keymap yeetube-mode-map
  :doc "Keymap for yeetube commands"
  "RET" #'yeetube-play
  "M-RET" #'yeetube-search
  "b" #'yeetube-browse-url
  "d" #'yeetube-download-video
  "D" #'yeetube-change-download-directory
  "a" #'yeetube-change-download-audio-format
  "p" #'yeetube-mpv-toggle-pause
  "v" #'yeetube-mpv-toggle-video
  "V" #'yeetube-mpv-toggle-no-video-flag
  "s" #'yeetube-save-video
  "P" #'yeetube-play-saved-video
  "r" #'yeetube-replay
  "q" #'quit-window)

(define-derived-mode yeetube-mode special-mode "Yeetube"
  "Yeetube mode."
  :interactive t
  (abbrev-mode 0)
  (display-line-numbers-mode 0)
  :lighter " yeetube-mode"
  :keymap yeetube-mode-map)

(provide 'yeetube)
;;; yeetube.el ends here
