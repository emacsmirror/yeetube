;;; yeetube-youtube.el --- YouTube backend for yeetube  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Thanos Apollo

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

;; YouTube implementation of the yeetube backend interface: request
;; construction for search, channel tabs and InnerTube pagination, plus
;; RSS feed parsing.  Page parsing itself lives in yeetube-scraper.el.

;;; Code:

(require 'cl-generic)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'xml)
(require 'dom)

(require 'yeetube-backend)
(require 'yeetube-scraper)

(define-obsolete-variable-alias 'yeetube-video-url
  'yeetube-youtube-video-url "2.4.0")
(defvar yeetube-youtube-video-url "https://youtube.com/watch?v="
  "URL used to play videos from.

You can change this value to an invidious instance.  Although yeetube
will still query youtube, `yeetube-play' will use the above url to play
videos from.")

(define-obsolete-variable-alias 'yeetube-playlist-url
  'yeetube-youtube-playlist-url "2.4.0")
(defvar yeetube-youtube-playlist-url "https://youtube.com/playlist?list="
  "URL used to play playlists from.

You can change this value to an invidious instance.  Although yeetube
will still query youtube, `yeetube-play' will use the above url to play
playlists from.")

(define-obsolete-variable-alias 'yeetube-invidious-instances
  'yeetube-youtube-invidious-instances "2.4.0")
(defvar yeetube-youtube-invidious-instances
  '("vid.puffyan.us" "inv.nadeko.net" "invidious.flokinet.to")
  "Invidious instances used by `yeetube-browse-url'.
Either a list to pick a random instance from, or a single
instance as a string.")

(define-obsolete-variable-alias 'yeetube-filter 'yeetube-youtube-filter "2.4.0")
(defcustom yeetube-youtube-filter "Relevance"
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

(defvar yeetube-youtube-filter-code-alist
  '(("Relevance" . "EgIQAQ%253D%253D")
    ("Date" . "CAISAhAB")
    ("Views" . "CAMSAhAB")
    ("Rating" . "CAESAhAB"))
  "YouTube's opaque sort filter codes, appended as &sp= parameter.")

(defconst yeetube-youtube--rss-feed-url
  "https://www.youtube.com/feeds/videos.xml?channel_id="
  "Base URL for YouTube RSS feeds.")

(defvar yeetube-youtube--client-version "2.20260414.01.00"
  "YouTube API client version for continuation requests.")

;;; URL construction

(defun yeetube-youtube--filter-code (filter)
  "Return the &sp= code for FILTER, nil when unknown."
  (cdr (assoc filter yeetube-youtube-filter-code-alist)))

(defun yeetube-youtube--channel-url (channel-id path)
  "Return a YouTube channel URL for CHANNEL-ID and PATH."
  (format "https://youtube.com/%s/%s"
          (string-remove-prefix "/" channel-id)
          (string-remove-prefix "/" path)))

;;; Requests

(cl-defmethod yeetube-backend-search-request ((_backend (eql 'youtube)) query)
  "Return the YouTube search request for QUERY.
The filter code from `yeetube-youtube-filter' is appended as the
&sp= parameter; the EU cookie-consent redirect is bypassed via the
Cookie header configured in `yeetube-request-headers'."
  (let ((code (yeetube-youtube--filter-code yeetube-youtube-filter)))
    (list :url (concat "https://youtube.com/search?q="
                       (url-hexify-string query)
                       (and code (format "&sp=%s" code))))))

(cl-defmethod yeetube-backend-channel-request ((_backend (eql 'youtube))
                                               channel what &optional query)
  "Return the YouTube request for CHANNEL's WHAT tab.
WHAT is `videos', `streams', or `search' with a QUERY string."
  (let ((path (pcase what
                ('videos "videos")
                ('streams "streams")
                ('search (format "search?query=%s" (url-hexify-string query)))
                (_ (error "Unknown channel content type: %s" what)))))
    (list :url (yeetube-youtube--channel-url channel path))))

(cl-defmethod yeetube-backend-continuation-request ((_backend (eql 'youtube))
                                                    continuation)
  "Return the InnerTube POST request for CONTINUATION."
  (let ((path (plist-get continuation :url)))
    (when (or (null path) (string-empty-p path))
      (user-error "Could not determine next page"))
    (list :url (concat "https://www.youtube.com" path)
          :method "POST"
          :headers '(("Content-Type" . "application/json"))
          :data (json-serialize
                 `((context (client (clientName . "WEB")
                                    (clientVersion . ,yeetube-youtube--client-version)))
                   (continuation . ,(plist-get continuation :token)))))))

;;; Parsers

(cl-defmethod yeetube-backend-parse-page ((_backend (eql 'youtube)))
  "Parse ytInitialData from the current buffer."
  (yeetube-scraper-parse))

(cl-defmethod yeetube-backend-parse-continuation ((_backend (eql 'youtube)))
  "Parse an InnerTube continuation response from the current buffer."
  (goto-char (point-min))
  (search-forward "{" nil t)
  (backward-char)
  (yeetube-scraper-parse-continuation-response
   (json-parse-buffer :object-type 'alist :array-type 'list)))

;;; Item URLs

(cl-defmethod yeetube-backend-item-url ((_backend (eql 'youtube)) id type)
  "Return the YouTube playback URL for item ID of TYPE."
  (concat (if (eq type 'playlist)
              yeetube-youtube-playlist-url
            yeetube-youtube-video-url)
          id))

(cl-defmethod yeetube-backend-browse-url ((backend (eql 'youtube)) id type)
  "Return an invidious URL for BACKEND's item ID of TYPE."
  (let ((instance (cond ((stringp yeetube-youtube-invidious-instances)
                         yeetube-youtube-invidious-instances)
                        (yeetube-youtube-invidious-instances
                         (seq-random-elt yeetube-youtube-invidious-instances))
                        (t (user-error "No invidious instances configured")))))
    (string-replace "youtube.com" instance
                    (yeetube-backend-item-url backend id type))))

;;; Channel input

(cl-defmethod yeetube-backend-read-channel ((_backend (eql 'youtube)))
  "Read a YouTube channel identifier, defaulting plain names to handles."
  (let ((channel (string-trim (read-string "Channel: "))))
    (cond ((string-empty-p channel)
           (user-error "No channel specified"))
          ((or (string-prefix-p "@" channel)
               (string-prefix-p "/" channel))
           channel)
          (t (format "@%s" channel)))))

;;; RSS feeds
;; Dormant infrastructure: consumed by `yeetube-display-feed' for the
;; future `yeetube-home' subscription view.  Not wired into any
;; interactive command yet: `yeetube-channel-videos' scrapes the HTML
;; videos tab instead, which is the only way to get duration/views and
;; to paginate past YouTube's 15-entry RSS cap.

(cl-defmethod yeetube-backend-feed-url ((_backend (eql 'youtube)) channel)
  "Return the YouTube RSS feed URL for CHANNEL, a browse ID."
  (concat yeetube-youtube--rss-feed-url channel))

(cl-defmethod yeetube-backend-parse-feed ((_backend (eql 'youtube)))
  "Parse a YouTube RSS feed from the current buffer."
  (yeetube-youtube--rss-parse-buffer))

(defun yeetube-youtube--rss-entry-text (entry tag)
  "Return text for direct child TAG in RSS ENTRY, or an empty string."
  (or (when-let* ((node (dom-child-by-tag entry tag)))
        ;; Not `dom-inner-text', which is new in Emacs 31.
        (string-trim (dom-texts node "")))
      ""))

(defun yeetube-youtube--rss-author-name (entry)
  "Return author name from RSS ENTRY, or an empty string."
  (or (when-let* ((author (dom-child-by-tag entry 'author))
                  (name (dom-child-by-tag author 'name)))
        (string-trim (dom-texts name "")))
      ""))

(defun yeetube-youtube--rss-channel-path (browse-id)
  "Return channel path for BROWSE-ID, or an empty string."
  (if (string-empty-p browse-id)
      ""
    (format "/channel/%s" browse-id)))

(defun yeetube-youtube--rss-entry-views (entry)
  "Return view count string from ENTRY's `media:statistics' tag.
YouTube nests `media:statistics' under `media:group', so this uses
recursive descent.  The view count is exposed as the `views'
attribute.  Returns an empty string when unavailable."
  (or (when-let* ((node (car (dom-by-tag entry 'media:statistics)))
                  (views (dom-attr node 'views)))
        views)
      ""))

(defun yeetube-youtube--rss-entry-thumbnail (entry video-id)
  "Return thumbnail URL for ENTRY, falling back to VIDEO-ID's default.
YouTube nests `media:thumbnail' under `media:group', so this uses
recursive descent.  Rewrites `hqdefault' to `mqdefault' because
hqdefault adds black bars at the top and bottom (NewPipeExtractor
does the same rewrite for the same reason)."
  (or (when-let* ((node (car (dom-by-tag entry 'media:thumbnail)))
                  (url (dom-attr node 'url))
                  ((not (string-empty-p url))))
        (string-replace "hqdefault" "mqdefault" url))
      (format "https://i.ytimg.com/vi/%s/default.jpg" video-id)))

(defun yeetube-youtube--rss-entry-item (entry)
  "Convert RSS ENTRY to a yeetube item plist."
  (let* ((id (yeetube-youtube--rss-entry-text entry 'yt:videoId))
         (browse-id (yeetube-youtube--rss-entry-text entry 'yt:channelId))
         (published (yeetube-youtube--rss-entry-text entry 'published))
         (date (or (and (not (string-empty-p published)) published)
                   (yeetube-youtube--rss-entry-text entry 'updated))))
    (and (not (string-empty-p id))
         (list :id id
               :title (yeetube-youtube--rss-entry-text entry 'title)
               :channel (yeetube-youtube--rss-author-name entry)
               :channel-id (yeetube-youtube--rss-channel-path browse-id)
               :browse-id browse-id
               :thumbnail-url (yeetube-youtube--rss-entry-thumbnail entry id)
               :views (yeetube-youtube--rss-entry-views entry)
               :duration ""
               :date date
               :type 'video))))

(defun yeetube-youtube--rss-parse-buffer ()
  "Parse current YouTube RSS feed buffer into yeetube item plists."
  (let* ((feed (car (xml-parse-region (point-min) (point-max))))
         (entries (and feed (dom-by-tag feed 'entry))))
    (delq nil (mapcar #'yeetube-youtube--rss-entry-item entries))))

(provide 'yeetube-youtube)
;;; yeetube-youtube.el ends here
