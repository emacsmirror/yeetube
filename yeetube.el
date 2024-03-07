;;; yeetube.el --- Scrape YouTube - Play with mpv & Download with yt-dlp |  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos
;; URL: https://thanosapollo.org/projects/yeetube/
;; Version: 2.1.4

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
;; displayed in a tabulated list.
;;
;; Core features include:
;;
;; - Executing queries on YouTube
;; - Playing video URLs using default mpv
;; - Bookmark/Save video url for future reference
;; - Download video using yt-dlp
;; - A standalone, minimalist yt-dlp frontend

;;; Code:

(require 'compat)
(require 'url)
(require 'tabulated-list)
(require 'cl-lib)
(require 'socks)
(require 'iimage)
(require 'url-handlers)

(require 'yeetube-mpv)

(defgroup yeetube nil
  "Youtube Front-End."
  :group 'external
  :prefix "yeetube-")

(defcustom yeetube-results-limit 20
  "Define a limit for search results."
  :type 'number
  :group 'yeetube)

(defcustom yeetube-play-function #'yeetube-mpv-play
  "Select media player function."
  :type 'function
  :group 'yeetube)

(defcustom yeetube-download-audio-format nil
  "Download videos as specified audio only formats."
  :type '(radio (const :tag "None" nil)
		(const :tag "AAC" "aac")
		(const :tag "ALAC" "alac")
		(const :tag "FLAC" "flac")
		(const :tag "M4A" "m4a")
		(const :tag "MP3" "mp3")
		(const :tag "OPUS" "opus")
		(const :tag "Vorbis" "vorbis")
		(const :tag "WAV" "wav"))
  :group 'yeetube)

(defcustom yeetube-download-directory (expand-file-name "Downloads" "~")
  "Default directory to downlaod videos."
  :type 'string
  :group 'yeetube)

(defcustom yeetube-filter "Relevance"
  "Sort search results for value.
Valid options include:
- \"Relevance\"
- \"Date\"
- \"Views\"
- \"Rating\""
  :type '(radio (const "Relevance")
		(const "Date")
		(const "Views")
		(const "Rating")))

(defcustom yeetube-default-sort-column nil
  "Column to sort the search results table."
  :type '(radio (const :tag "None" nil)
		(const "Title")
                (const "Views")
                (const "Duration")
                (const "Channel"))
  :group 'yeetube)

(defcustom yeetube-default-sort-ascending nil
  "Whether to sort the search results in ascending order."
  :type 'boolean
  :group 'yeetube)

(defcustom yeetube-enable-tor nil
  "Enable routing through tor."
  :type 'boolean
  :group 'yeetube)

(defgroup yeetube-faces nil
  "Faces used by yeetube."
  :group 'yeetube
  :tag "Yeetube Faces"
  :prefix 'yeetube-face)

(defcustom yeetube-thumbnail-size '(90 . 90)
  "Thumbnail size (height width)."
  :type '(cons integer integer)
  :group 'yeetube)

(defcustom yeetube-display-thumbnails t
  "When t, fetch & display thumbnails.

Disabled by default, still an experimental feature that a user should
opt-in.  Note that when enabled the thumbnail images will be downloaded
on `temporary-file-directory'."
  :type 'boolean
  :group 'yeetube)

(defface yeetube-face-header-query
  '((t :inherit font-lock-function-name-face))
  "Face used for the video published date."
  :group 'yeetube-faces)

(defface yeetube-face-duration
  '((t :inherit font-lock-string-face))
  "Face used for the video duration."
  :group 'yeetube-faces)

(defface yeetube-face-view-count
  '((t :inherit font-lock-keyword-face))
  "Face used for the video view count."
  :group 'yeetube-faces)

(defface yeetube-face-title
  '((t :inherit font-lock-variable-use-face))
  "Face used for video title."
  :group 'yeetube-faces)

(defface yeetube-face-channel
  '((t :inherit font-lock-function-call-face))
  "Face used for video channel name."
  :group 'yeetube-faces)

(defface yeetube-face-date
  '((t :inherit font-lock-doc-face))
  "Face used for published date."
  :group 'yeetube-faces)

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

(defvar yeetube-search-history nil
  "History of search terms.")

(defvar yeetube-url "https://youtube.com/watch?v="
  "URL used to play videos from.

You can change this value to an invidious instance.  Although yeetube
will still query youtube, `yeetube-play' will use the above url to play
videos from.")

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
  (cl-getf (tabulated-list-get-id) keyword))

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
    (funcall yeetube-play-function video-url)
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
    (funcall yeetube-play-function url)
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
    (funcall yeetube-play-function (cdr (assoc video yeetube-saved-videos)))
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

(defun yeetube--wget-thumbnail (torsocks url &optional output)
  "Get thumbnail using `wget' from URL.

If TORSOCKS is non-nil, use torsocks to download URL.
URL is the URL to download.
OUTPUT is the output file name."
  (let ((wget-exec (executable-find "wget")))
    (unless wget-exec
      (error "Please install `wget' to download videos"))
    (let ((command (if torsocks
		       (format "%s %s %s -O %s.jpg" (executable-find "torsocks") wget-exec
			       (shell-quote-argument url) (shell-quote-argument output))
		     (format "%s %s -O %s.jpg" wget-exec (shell-quote-argument url)
			     (shell-quote-argument output)))))
      (call-process-shell-command command nil 0))))


(cl-defun yeetube-get-thumbnails (content)
  "Download thumbnails for CONTENT using `wget'.

This is used to download thumbnails from `yeetube-content'."
  (interactive)
  (when yeetube-display-thumbnails
    (let ((default-directory temporary-file-directory))
      (cl-loop for item in content
	       do (let ((thumbnail (plist-get item :thumbnail))
			(videoid (plist-get item :videoid)))
		    (unless (file-exists-p (expand-file-name (concat videoid ".jpg")))
		      (yeetube--wget-thumbnail yeetube-enable-tor thumbnail videoid)))))))

(defvar yeetube-filter-code-alist
  '(("Relevance" . "EgIQAQ%253D%253D")
    ("Date" . "CAISAhAB")
    ("Views" . "CAMSAhAB")
    ("Rating" . "CAESAhAB"))
  "Filter codes.")

(defvar yeetube-request-headers
  '(("Accept-Language" . "Accept-Language: en-US,en;q=0.9")
    ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8")
    ("User-Agent" . "Mozilla/5.0 (Windows NT 10.0; rv:122.0) Gecko/20100101 Firefox/122.0"))
  "HTTP Request extra headers.")

(defun yeetube-get-filter-code (filter)
  "Get FILTER code for sorting search results."
  (cdr (assoc filter yeetube-filter-code-alist)))

(defmacro yeetube-with-tor-socks (&rest body)
  "Execute BODY with torsocks."
  `(let ((url-gateway-method 'socks)
         (socks-noproxy '("localhost"))
         (socks-server '("Default server" "127.0.0.1" 9050 5)))
     ,@body))

(defun yeetube--callback (status)
  "Yeetube callback handling STATUS."
  (let ((url-buffer (current-buffer)))
    (unwind-protect
        (if-let ((err (plist-get :error status)))
            (message "Error %s in retrieving yeetube results: %S" (car err) (cdr err))
          (with-temp-buffer
            (set-buffer-multibyte t)
            (url-insert url-buffer)
            (decode-coding-region (point-min) (point-max) 'utf-8)
            (goto-char (point-min))
            (yeetube-get-content)
            (yeetube-get-thumbnails yeetube-content)) ;; download thumbnails
          (pop-to-buffer-same-window "*yeetube*")
          (yeetube-mode))
      (kill-buffer url-buffer))))

(defun yeetube-display-content-from-url (url)
  "Display the video results from URL."
  (let* ((url-request-extra-headers yeetube-request-headers))
    (if yeetube-enable-tor
        (yeetube-with-tor-socks
         (url-queue-retrieve url #'yeetube--callback nil 'silent 'inhibit-cookies))
      (url-queue-retrieve url #'yeetube--callback nil 'silent 'inhibit-cookies))))

(defun yeetube-read-query ()
  "Interactively read a search term."
  (read-string "Yeetube Search: " nil 'yeetube-search-history))

;;;###autoload
(defun yeetube-search (query)
  "Search for QUERY."
  (interactive (list (yeetube-read-query)))
  (yeetube-display-content-from-url
   (format "https://youtube.com/search?q=%s&sp=%s"
           (url-hexify-string query)
           (yeetube-get-filter-code yeetube-filter))))

(defun yeetube-channel-id-at-point ()
  "Return the channel name for the video at point."
  (if-let ((entry (tabulated-list-get-entry)))
      (get-text-property 0 :channel-id (aref entry 4))
    (error "No video at point")))

(defun yeetube-channel-videos (channel-id)
  "View (some) videos for the channel with CHANNEL-ID."
  (interactive (list (yeetube-channel-id-at-point)))
  (yeetube-display-content-from-url (format "https://youtube.com/%s/videos" channel-id)))

(defun yeetube-channel-search (channel-id query)
  "Search channel with CHANNEL-ID for vidoes matching QUERY."
  (interactive (list (yeetube-channel-id-at-point) (yeetube-read-query)))
  (yeetube-display-content-from-url
   (format "https://youtube.com/%s/search?query=%s"
           channel-id
           (url-hexify-string query))))

;;;###autoload
(defun yeetube-browse-url ()
  "Open URL for video at point, using an invidious instance."
  (interactive)
  (let ((invidious-instance (+ 1 (random (length yeetube-invidious-instances)))))
    (browse-url
     (replace-regexp-in-string "youtube.com"
			       (nth invidious-instance yeetube-invidious-instances)
			       (yeetube-get-url)))))

(cl-defun yeetube-scrape-item (&key item (item-start "text") item-end (substring-start 3) substring-end)
  "Scrape ITEM from YouTube.com.

Video result starts with videorenderer.
Search back to videorenderer (start of video results),
then for item.

ITEM-START is the start of the information for item.
ITEM-END is the end of the item information.
SUBSTRING-START is the start of the string to return, integer.
SUBSTRING-END is the end of the string to return, interger."
  (search-backward "videorenderer" nil t)
  (search-forward item nil t)
  (search-forward item-start nil t)
  (let ((item (buffer-substring (+ (point) substring-start)
				(- (search-forward item-end) substring-end))))
    item))

(defun yeetube-view-count-format (string)
  "Add commas for STRING."
  (let* ((string (replace-regexp-in-string "[^0-9]" "" string))
         (len (length string))
         (result ""))
    (cl-loop for i from 0 to (1- len)
             do (setf result (concat (substring string (- len i 1) (- len i)) result))
             if (and (> (- len (1+ i)) 0)
                     (= (% (1+ i) 3) 0))
             do (setf result (concat "," result)))
    result))

(defun yeetube-get-content ()
  "Get content from youtube."
  (setf yeetube-content nil)
  (while (and (< (length yeetube-content) yeetube-results-limit)
              (search-forward "videorenderer" nil t))
    (search-forward "videoid")
    (let ((videoid (buffer-substring (+ (point) 3)
                                     (- (search-forward ",") 2))))
      (unless (member videoid (car yeetube-content))
        (save-excursion
          (let ((title (yeetube-scrape-item :item "title" :item-end ",\"" :substring-end 5))
                (view-count (yeetube-scrape-item :item "viewcounttext" :item-end " " :substring-end 0))
                (video-duration (yeetube-scrape-item :item "lengthtext" :item-end "}," :substring-end 3))
                (channel (yeetube-scrape-item :item "longbylinetext" :item-end "," :substring-end 2))
                (channel-id (yeetube-scrape-item :item "canonicalBaseUrl" :item-start "/"
                                                 :substring-start 0 :item-end "\"" :substring-end 1))
                (thumbnail (yeetube-scrape-item :item "thumbnail" :item-start "url" :item-end ".jpg" :substring-end 0))
                (date (yeetube-scrape-item :item "publishedtimetext" :item-end ",\"" :substring-end 4)))
            (push (list :title title
                        :videoid videoid
                        :view-count (yeetube-view-count-format view-count)
                        :duration video-duration
                        :channel (propertize channel :channel-id channel-id)
                        :thumbnail (replace-regexp-in-string "hq720" "default" thumbnail)
                        :date (replace-regexp-in-string "Streamed " "" date)
                        :image (if yeetube-display-thumbnails
                                   (format "[[%s.jpg]]" (expand-file-name
                                                         videoid
                                                         temporary-file-directory))
                                 "disabled"))
                  yeetube-content)))))))

(add-variable-watcher 'yeetube-saved-videos #'yeetube-update-saved-videos-list)

;; View thumbnail using eww
(defun yeetube-view-thumbnail ()
  "Open URL using eww in a new buffer."
  (interactive)
  (eww-browse-url (yeetube-get :thumbnail)))


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
  (unless (executable-find "yt-dlp")
    (error "Executable for yt-dlp not found.  Please install yt-dlp"))
  (let* ((tor-command (when yeetube-enable-tor (executable-find "torsocks")))
         (name-command (when name (format "-o %s" (shell-quote-argument name))))
         (format-command (when audio-format
			   (format "--extract-audio --audio-format %s" (shell-quote-argument audio-format))))
         (command (mapconcat 'identity (delq nil
					     (list tor-command
						   (executable-find "yt-dlp")
						   (shell-quote-argument url)
						   name-command format-command))
			     " ")))
    (call-process-shell-command command nil 0)))

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

(defun yeetube-propertize-vector (content &rest fields-face-pairs)
  "Create a vector with each item propertized with its corresponding face.

CONTENT is a list of strings.
FIELDS-FACE-PAIRS is a list of fields and faces."
  (apply #'vector
         (cl-loop for (field face) on fields-face-pairs by #'cddr
                  collect (propertize (cl-getf content field) 'face face))))

;; Yeetube Mode
(defvar-keymap yeetube-mode-map
  :doc "Keymap for yeetube commands"
  "RET" #'yeetube-play
  "M-RET" #'yeetube-search
  "b" #'yeetube-browse-url
  "d" #'yeetube-download-video
  "D" #'yeetube-download-change-directory
  "a" #'yeetube-download-change-audio-format
  "p" #'yeetube-mpv-toggle-pause
  "v" #'yeetube-mpv-toggle-video
  "V" #'yeetube-mpv-toggle-no-video-flag
  "s" #'yeetube-save-video
  "P" #'yeetube-play-saved-video
  "r" #'yeetube-replay
  "t" #'yeetube-view-thumbnail
  "T" #'yeetube-mpv-toggle-torsocks
  "C-q" #'yeetube-mpv-change-video-quality
  "q" #'quit-window)

(defun yeetube--sort-views (a b)
  "PREDICATE for function `sort'.

Used by `tabulated-list-format' to sort the \"Views\"
column.

A and B are vectors."
  (< (string-to-number (replace-regexp-in-string "," "" (aref (cadr a) 1)))
     (string-to-number (replace-regexp-in-string "," "" (aref (cadr b) 1)))))

(defun yeetube--sort-duration (a b)
  "PREDICATE for function `sort'.

Used by `tabulated-list-format' to sort the \"Duration\"
column.

A and B are vectors."
  (< (string-to-number (replace-regexp-in-string ":" "" (aref (cadr a) 2)))
     (string-to-number (replace-regexp-in-string ":" "" (aref (cadr b) 2)))))

(defun yeetube--sort-date (a b)
  "PREDICATE for function `sort'.

Used by variable `tabulated-list-format' to sort the \"Date\"
column.

A and B are vectors."
  (let* ((intervals '("second" "minute" "hour" "day" "week" "month" "year"))
         (split-a (split-string (replace-regexp-in-string "s" "" (aref (cadr a) 3))))
         (split-b (split-string (replace-regexp-in-string "s" "" (aref (cadr b) 3))))
         (units-a (length (member (nth 1 split-a) intervals)))
         (units-b (length (member (nth 1 split-b) intervals))))
    (if (= units-a units-b)
      (< (string-to-number (nth 0 split-a)) (string-to-number (nth 0 split-b)))
      (> units-a units-b))))

;; Modified from iimage.el for hardcoded width/height
(defun yeetube-iimage-mode-buffer (arg)
  "Display images if ARG is non-nil, undisplay them otherwise."
  (let ((image-path (cons default-directory iimage-mode-image-search-path))
	file)
    (with-silent-modifications
      (save-excursion
        (dolist (pair iimage-mode-image-regex-alist)
          (goto-char (point-min))
          (while (re-search-forward (car pair) nil t)
            (when (and (setq file (match-string (cdr pair)))
                       (setq file (locate-file file image-path)))
              (if arg
                  (add-text-properties
                   (match-beginning 0) (match-end 0)
                   `(display
                     ,(create-image file nil nil
                                    :max-width (car yeetube-thumbnail-size)
				    :max-height (cdr yeetube-thumbnail-size)))
                (remove-list-of-text-properties
                 (match-beginning 0) (match-end 0)
                 '(display modification-hooks)))))))))))

(define-derived-mode yeetube-mode tabulated-list-mode "Yeetube"
  "Yeetube mode."
  :keymap yeetube-mode-map
  (setf tabulated-list-format
        [("Title" 60 t)
         ("Views" 11 yeetube--sort-views)
         ("Duration" 9 yeetube--sort-duration)
	 ("Date" 13 yeetube--sort-date)
         ("Channel" 12 t)
	 ("Thumbnail" 0 t)]
	tabulated-list-entries
	(cl-map 'list
		(lambda (content)
                  (list content
			(yeetube-propertize-vector content
                                                   :title 'yeetube-face-title
                                                   :view-count 'yeetube-face-view-count
                                                   :duration 'yeetube-face-duration
						   :date 'yeetube-face-date
                                                   :channel 'yeetube-face-channel
						   :image nil)))
		(reverse yeetube-content))
	tabulated-list-sort-key (cons yeetube-default-sort-column
                                      yeetube-default-sort-ascending))
  (display-line-numbers-mode 0)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (yeetube-iimage-mode-buffer t))

(provide 'yeetube)
;;; yeetube.el ends here
