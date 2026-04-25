;;; yeetube.el --- Scrape YouTube, Play with mpv & Download with yt-dlp  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos
;; URL: https://thanosapollo.org/projects/yeetube/
;; Version: 2.1.12

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2") (transient "0.7.2"))

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
;; - Executing YouTube queries
;; - Playing videos, by default using MPV
;; - Downloading videos, using yt-dlp
;; - Bookmarking and saving video URLs
;; - A standalone, minimalist yt-dlp frontend

;;; Code:

(require 'compat)
(require 'url)
(require 'cl-lib)
(require 'socks)
(require 'url-handlers)
(require 'xdg)
(require 'json)

(require 'yeetube-scraper)
(require 'yeetube-ui)
(require 'yeetube-mpv)
(require 'yeetube-download)
(require 'yeetube-menu)

(defgroup yeetube nil
  "Youtube Front-End."
  :group 'external
  :prefix "yeetube-")

(defcustom yeetube-torsocks-program (executable-find "torsocks")
  "Path for torsocks executable."
  :type 'string
  :group 'yeetube)

(defcustom yeetube-ytdlp-program (executable-find "yt-dlp")
  "Path for yt-dlp executable."
  :type 'string
  :group 'yeetube)

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

(defcustom yeetube-download-directory (or (xdg-user-dir "DOWNLOAD") (getenv "HOME"))
  "Default directory to download videos."
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
		(const "Rating"))
  :group 'yeetube)

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

(defcustom yeetube-enable-emojis t
  "Enable emojis in *yeetube* buffer."
  :type 'boolean
  :group 'yeetube)

(defcustom yeetube-pop-to-same-window-p t
  "When non-nil, create *yeetube* buffer at the same window."
  :type 'boolean
  :group 'yeetube)

(defcustom yeetube-thumbnail-size '(120 . 90)
  "Thumbnail size (width . height)."
  :type '(cons integer integer)
  :group 'yeetube)

(defcustom yeetube-display-thumbnails-p t
  "When non-nil, fetch & display thumbnails."
  :type 'boolean
  :group 'yeetube)

(defvar yeetube-invidious-instances
  '("vid.puffyan.us" "inv.nadeko.net" "invidious.flokinet.to")
  "List of invidious instances.")

(defvar yeetube-content nil
  "Tabulated-list rows (ID VECTOR) pairs.")

(defvar yeetube-items nil
  "List of scraped item plists.")

(defvar-local yeetube--continuation nil
  "Continuation plist for pagination.")

(defvar-local yeetube--results-limit nil
  "Buffer-local results limit.")

(defvar-local yeetube--current-url nil
  "URL that produced the current results.
Used to re-fetch when the results limit changes.")

(defvar yeetube-saved-videos nil
  "Saved/bookmarked video urls.")

(defvar yeetube-history nil
  "Stored urls & titles of recently played content.")

(defvar yeetube-search-history nil
  "History of search terms.")

(defvar yeetube-video-url "https://youtube.com/watch?v="
  "URL used to play videos from.

You can change this value to an invidious instance.  Although yeetube
will still query youtube, `yeetube-play' will use the above url to play
videos from.")

(defvar yeetube-playlist-url "https://youtube.com/playlist?list="
  "URL used to play playlists from.

You can change this value to an invidious instance.  Although yeetube
will still query youtube, `yeetube-play' will use the above url to play
videos from.")

(defvar-local yeetube--channel-id nil
  "Current channel ID.")

(defvar yeetube-filter-code-alist
  '(("Relevance" . "EgIQAQ%253D%253D")
    ("Date" . "CAISAhAB")
    ("Views" . "CAMSAhAB")
    ("Rating" . "CAESAhAB"))
  "YouTube's opaque sort filter codes, appended as &sp= parameter.")

(defvar yeetube-request-headers
  '(("Accept-Language" . "en-US,en;q=0.9")
    ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8")
    ("User-Agent" . "Mozilla/5.0 (Windows NT 10.0; rv:126.0) Gecko/20100101 Firefox/126.0"))
  "HTTP Request extra headers.")

(defvar yeetube--client-version "2.20260414.01.00"
  "YouTube API client version for continuation requests.")


;;; Helpers

(defun yeetube--find-item (id)
  "Find item plist with ID in `yeetube-items'."
  (cl-find id yeetube-items
           :key (lambda (item) (plist-get item :id))
           :test #'equal))

(defun yeetube-get-filter-code (filter)
  "Get FILTER code for sorting search results."
  (cdr (assoc filter yeetube-filter-code-alist)))

(defmacro yeetube-with-tor-socks (&rest body)
  "Execute BODY with torsocks."
  `(let ((url-gateway-method 'socks)
         (socks-noproxy '("localhost"))
         (socks-server '("Default server" "127.0.0.1" 9050 5)))
     ,@body))

(defun yeetube-get-url (&optional id type)
  "Get video or playlist url for entry ID, adjusted for TYPE."
  (let* ((id (or id (tabulated-list-get-id)))
         (item (yeetube--find-item id))
         (type (or type (plist-get item :type))))
    (format "%s%s" (if (eq type 'video)
                       yeetube-video-url
                     yeetube-playlist-url)
            id)))

(defun yeetube-channel-id-at-point ()
  "Return yeetube channel id at point."
  (let* ((id (tabulated-list-get-id))
         (item (yeetube--find-item id)))
    (plist-get item :channel-id)))

(defun yeetube-read-query ()
  "Interactively read a search term."
  (read-string "Yeetube Search: " nil 'yeetube-search-history))


;;; Playback

;;;###autoload
(defun yeetube-play ()
  "Play video at point in *yeetube* buffer."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (item (yeetube--find-item id))
         (url (yeetube-get-url id (plist-get item :type)))
         (title (plist-get item :title))
         (proc (apply yeetube-play-function url
                      (when yeetube-mpv-modeline-mode (list title)))))
    (when (processp proc)
      (process-put proc :now-playing title))
    (push (list :url url :title title) yeetube-history)
    (message "Playing: %s" title)))

;;;###autoload
(defun yeetube-copy-url ()
  "Copy entry URL at point."
  (interactive)
  (cl-assert (derived-mode-p 'yeetube-mode) nil "Yeetube mode not enabled")
  (let ((url (yeetube-get-url)))
    (kill-new url)
    (message "Copied url: %s" url)))

;;;###autoload
(defun yeetube-replay ()
  "Select entry from history to replay.

Select entry title from `yeetube-history' and play corresponding URL."
  (interactive)
  (let* ((titles (mapcar (lambda (entry) (plist-get entry :title)) yeetube-history))
         (selected (completing-read "Replay: " titles))
         (selected-entry (cl-find-if (lambda (entry)
				       (string= selected (plist-get entry :title)))
				     yeetube-history))
	 (title (plist-get selected-entry :title))
         (url (plist-get selected-entry :url)))
    (funcall yeetube-play-function url (when yeetube-mpv-modeline-mode title))
    (message "Replaying: %s" selected)))

;;;###autoload
(defun yeetube-browse-url ()
  "Open URL for video at point, using an invidious instance."
  (interactive)
  (let ((invidious-instance (cond ((and (listp yeetube-invidious-instances)
					(length> yeetube-invidious-instances 1))
				   (nth (random (length yeetube-invidious-instances))
					yeetube-invidious-instances))
				  ((and (listp yeetube-invidious-instances)
					(length= yeetube-invidious-instances 1))
				   (car yeetube-invidious-instances))
				  ((stringp yeetube-invidious-instances)
				   yeetube-invidious-instances))))
    (browse-url
     (replace-regexp-in-string "youtube.com" invidious-instance (yeetube-get-url)))))


;;; Bookmarks

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
(defun yeetube-save-video (arg)
  "Save url at point.

If ARG is non-nil, save as a playlist URL."
  (interactive "P")
  (yeetube-load-saved-videos)
  (let ((name (read-string "Save as: "))
	(url (yeetube-get-url (tabulated-list-get-id) (if arg 'playlist 'video))))
    (push (cons name url) yeetube-saved-videos)))

;; We could use keywords here, but it would break users saved videos
;; from previous versions.
;;;###autoload
(defun yeetube-play-saved-video ()
  "Select & Play a saved video."
  (interactive)
  (yeetube-load-saved-videos)
  (let* ((video (completing-read "Select video: " yeetube-saved-videos nil t))
	 (url (cdr (assoc video yeetube-saved-videos)))
	 (title (car (assoc video yeetube-saved-videos))))
    (funcall yeetube-play-function url (when yeetube-mpv-modeline-mode title))
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
  "Clear all saved videos from yeetube."
  (interactive)
  (let ((clear-saved (y-or-n-p "Delete saved?")))
    (when clear-saved
      (setf yeetube-saved-videos nil))))

(defun yeetube-update-saved-videos-list (_symbol new-value _where _environment)
  "Update saved videos list file.

This is a variable watcher function that writes NEW-VALUE to the saved
videos file whenever `yeetube-saved-videos' changes.  _SYMBOL is the
variable being watched, _WHERE indicates the type of change, and
_ENVIRONMENT is the lexical environment."
  (let ((file-path (concat user-emacs-directory "yeetube")))
    (with-temp-buffer
      (insert (pp-to-string new-value))
      (write-region (point-min) (point-max) file-path))))

(add-variable-watcher 'yeetube-saved-videos #'yeetube-update-saved-videos-list)


;;; Download

;;;###autoload
(defun yeetube-download-video (&optional url)
  "Download entry at point in *yeetube* buffer with yt-dlp.

Content will be downloaded at `yeetube-download-directory'.
Optionally, provide custom own URL."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (item (yeetube--find-item id))
         (type (plist-get item :type))
         (url (or url (yeetube-get-url id type)))
         (title (or (plist-get item :title) "Unknown")))
    (when (string-prefix-p "http" url)
      (let ((default-directory yeetube-download-directory))
        (yeetube-download--ytdlp url nil yeetube-download-audio-format)
        (message "Downloading: '%s' at '%s'" title yeetube-download-directory)))))


;;; Search & Callbacks

(defun yeetube--callback (status)
  "Yeetube callback handling STATUS."
  (let ((url-buffer (current-buffer))
        (pop-fn (if yeetube-pop-to-same-window-p
                    #'pop-to-buffer-same-window
                  #'pop-to-buffer)))
    (unwind-protect
        (unless (plist-get status :error)
          (let* ((limit (with-current-buffer (get-buffer-create "*yeetube*")
                          (or yeetube--results-limit yeetube-results-limit)))
                 (result (with-temp-buffer
                           (set-buffer-multibyte t)
                           (url-insert url-buffer)
                           (decode-coding-region (point-min) (point-max) 'utf-8)
                           (yeetube-scraper-parse)))
                 (items (plist-get result :items))
                 (continuation (plist-get result :continuation)))
            (when items
              (funcall pop-fn "*yeetube*")
              (yeetube-mode)
              (setq yeetube-items items)
              (setq-local yeetube--continuation continuation)
              (setq-local yeetube--results-limit limit)
              (yeetube-ui-render items)
              (yeetube-ui-fetch-thumbnails items "*yeetube*")
              (when (and continuation (< (length items) limit))
                (yeetube--auto-paginate limit)))))
      (kill-buffer url-buffer))))

(defun yeetube-display-content-from-url (url)
  "Display the video results from URL."
  (with-current-buffer (get-buffer-create "*yeetube*")
    (setq-local yeetube--current-url url)
    (let ((url-request-extra-headers yeetube-request-headers))
      (if yeetube-enable-tor
          (yeetube-with-tor-socks
           (url-retrieve url #'yeetube--callback nil 'silent 'inhibit-cookies))
	(url-retrieve url #'yeetube--callback nil 'silent 'inhibit-cookies)))))

;;;###autoload
(defun yeetube-search (query)
  "Search for QUERY."
  (interactive (list (yeetube-read-query)))
  (pop-to-buffer-same-window "*yeetube*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Loading..." 'face 'bold-italic)))
  (yeetube-display-content-from-url
   ;; ucbcb=1 bypasses EU cookie consent redirect
   (format "https://youtube.com/search?q=%s&ucbcb=1%s"
           (url-hexify-string query)
           (if yeetube-filter
	       (format "&sp=%s" (yeetube-get-filter-code yeetube-filter))
	     ""))))


;;; Pagination

(defun yeetube-set-results-limit (limit)
  "Set the results limit for the current buffer to LIMIT.
When called from the *yeetube* buffer, re-fetches with the new limit."
  (interactive "nResults limit: ")
  (setq-local yeetube--results-limit limit)
  (if yeetube--current-url
      (progn
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "Loading..." 'face 'bold-italic)))
        (yeetube-display-content-from-url yeetube--current-url))
    (message "Results limit set to %d" limit)))

(defun yeetube--auto-paginate (limit)
  "Automatically fetch next page if current items are below LIMIT."
  (when (and yeetube--continuation
             (< (length yeetube-items) limit))
    (yeetube-next-page)))

(defun yeetube-next-page ()
  "Fetch and append the next page of results."
  (interactive)
  (unless yeetube--continuation
    (user-error "No more results"))
  (let* ((token (plist-get yeetube--continuation :token))
         (api-path (plist-get yeetube--continuation :url))
         (endpoint (concat "https://www.youtube.com" api-path))
         (url-request-method "POST")
         (url-request-extra-headers
          (append yeetube-request-headers
                  '(("Content-Type" . "application/json"))))
         (url-request-data
          (json-serialize
           `((context (client (clientName . "WEB")
                              (clientVersion . ,yeetube--client-version)))
             (continuation . ,token)))))
    (if yeetube-enable-tor
        (yeetube-with-tor-socks
         (url-retrieve endpoint #'yeetube--pagination-callback nil 'silent 'inhibit-cookies))
      (url-retrieve endpoint #'yeetube--pagination-callback nil 'silent 'inhibit-cookies))))

(defun yeetube--pagination-callback (status)
  "Handle pagination response with STATUS."
  (let ((url-buffer (current-buffer)))
    (unwind-protect
        (unless (plist-get status :error)
          (let* ((result (with-temp-buffer
                           (set-buffer-multibyte t)
                           (url-insert url-buffer)
                           (decode-coding-region (point-min) (point-max) 'utf-8)
                           (goto-char (point-min))
                           (search-forward "{" nil t)
                           (backward-char)
                           (yeetube-scraper-parse-continuation-response
                            (json-parse-buffer :object-type 'alist
                                               :array-type 'list))))
                 (items (plist-get result :items))
                 (continuation (plist-get result :continuation)))
            (when items
              (with-current-buffer "*yeetube*"
                (setq yeetube-items (append yeetube-items items))
                (setq-local yeetube--continuation continuation)
                (yeetube-ui-append items)
                (yeetube-ui-fetch-thumbnails items "*yeetube*")
                (when (and continuation
                           (< (length yeetube-items)
                              (or yeetube--results-limit yeetube-results-limit)))
                  (yeetube--auto-paginate
                   (or yeetube--results-limit yeetube-results-limit)))))))
      (kill-buffer url-buffer))))


;;; Channel browsing

(defun yeetube-channel-videos (&optional channel-id)
  "View videos for the channel with CHANNEL-ID."
  (interactive (list (or (yeetube-channel-id-at-point)
			  (format "@%s" (read-string "Channel: ")))))
  (with-current-buffer (get-buffer-create "*yeetube*")
    (setf yeetube--channel-id (substring channel-id 2))
    (yeetube-display-content-from-url
     (format "https://youtube.com/%s/videos?ucbcb=1" channel-id))))

(defun yeetube-channel-streams (&optional channel-id)
  "View streams for the channel with CHANNEL-ID."
  (interactive (list (or (yeetube-channel-id-at-point)
			  (format "@%s" (read-string "Channel: ")))))
  (with-current-buffer (get-buffer-create "*yeetube*")
    (setf yeetube--channel-id (substring channel-id 2))
    (yeetube-display-content-from-url (format "https://youtube.com/%s/streams?ucbcb=1" channel-id))))

(defun yeetube-channel-search (channel-id query)
  "Search channel with CHANNEL-ID for videos matching QUERY."
  (interactive (list (yeetube-channel-id-at-point) (yeetube-read-query)))
  (yeetube-display-content-from-url
   (format "https://youtube.com/%s/search?query=%s&ucbcb=1"
           channel-id (url-hexify-string query))))


;;; Mode

(defvar-keymap yeetube-mode-map
  :doc "Keymap for yeetube commands"
  "RET" #'yeetube-play
  "M-RET" #'yeetube-search
  "b" #'yeetube-browse-url
  "c" #'yeetube-channel-videos
  "C" #'yeetube-copy-url
  "d" #'yeetube-download-video
  "D" #'yeetube-download-change-directory
  "a" #'yeetube-download-change-audio-format
  "p" #'yeetube-mpv-toggle-pause
  "v" #'yeetube-mpv-toggle-video
  "V" #'yeetube-mpv-toggle-no-video-flag
  "s" #'yeetube-save-video
  "L" #'yeetube-channel-streams
  "P" #'yeetube-play-saved-video
  "r" #'yeetube-replay
  "T" #'yeetube-mpv-toggle-torsocks
  "C-q" #'yeetube-mpv-change-video-quality
  "M-n" #'yeetube-next-page
  "C-c l" #'yeetube-set-results-limit
  "h" #'yeetube-buffer-menu
  "q" #'quit-window)

(define-derived-mode yeetube-mode tabulated-list-mode "Yeetube"
  "Yeetube mode."
  :keymap yeetube-mode-map
  (setq-local truncate-string-ellipsis " ")
  (display-line-numbers-mode 0)
  (when (and (fboundp 'emojify-mode)
	     yeetube-enable-emojis)
    (emojify-mode 1)))

;;;###autoload
(defalias 'yeetube #'yeetube-menu)

(provide 'yeetube)
;;; yeetube.el ends here
