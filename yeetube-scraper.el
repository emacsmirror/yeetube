;;; yeetube-scraper.el --- Pure YouTube JSON parsing for yeetube  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos
;; URL: https://thanosapollo.org/projects/yeetube/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Pure data extraction from YouTube's ytInitialData JSON.
;; All private functions are side-effect free.
;; The public entry point `yeetube-scraper-parse' reads from the
;; current buffer but restores point via `save-excursion'.
;; Input: parsed JSON (alists).  Output: plists.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun yeetube-scraper--text-from-object (obj)
  "Return text from OBJ, which may be a `simpleText' or `runs' wrapper.
YouTube returns text fields in two forms depending on layout: a
single `simpleText' string, or a `runs' array of segments to be
concatenated.  Returns nil when neither shape is present."
  (cond ((null obj) nil)
        ((alist-get 'simpleText obj))
        ((alist-get 'runs obj)
         (mapconcat (lambda (run) (or (alist-get 'text run) ""))
                    (alist-get 'runs obj)))))

(defun yeetube-scraper--byline-runs (renderer)
  "Return the first non-empty runs list among the byline candidates.
Mirrors the fallback chain YouTube uses across layouts:
`longBylineText' (search/channel pages), `ownerText' (some
playlist views), `shortBylineText' (compact contexts)."
  (cl-some (lambda (key)
             (alist-get 'runs (alist-get key renderer)))
           '(longBylineText ownerText shortBylineText)))

(defun yeetube-scraper--extract-video (renderer)
  "Extract a video plist from a videoRenderer RENDERER alist."
  (let* ((title (yeetube-scraper--text-from-object
                 (alist-get 'title renderer)))
         (views (yeetube-scraper--text-from-object
                 (alist-get 'viewCountText renderer)))
         (duration (yeetube-scraper--text-from-object
                    (alist-get 'lengthText renderer)))
         (date-raw (yeetube-scraper--text-from-object
                    (alist-get 'publishedTimeText renderer)))
         (date (when date-raw (string-replace "Streamed " "" date-raw)))
         (byline-runs (yeetube-scraper--byline-runs renderer))
         (channel (alist-get 'text (car byline-runs)))
         (browse-endpoint (alist-get 'browseEndpoint
				     (alist-get 'navigationEndpoint (car byline-runs))))
         (channel-id (alist-get 'canonicalBaseUrl browse-endpoint))
         (browse-id (alist-get 'browseId browse-endpoint))
         (thumbs (alist-get 'thumbnails (alist-get 'thumbnail renderer)))
         (thumb-url (yeetube-scraper--thumbnail-url
                     (alist-get 'videoId renderer) thumbs)))
    (list :id (alist-get 'videoId renderer)
          :title (or title "")
          :views (or views "")
          :duration (or duration "")
          :date (or date "")
          :channel (or channel "")
          :channel-id (or channel-id "")
          :browse-id (or browse-id "")
          :thumbnail-url (or thumb-url "")
          :type 'video)))

(defun yeetube-scraper--thumbnail-url (video-id thumbnails)
  "Build a small thumbnail URL for VIDEO-ID from THUMBNAILS list.
Falls back to the predictable default.jpg URL pattern.
Normalises any hq720 / hqdefault / mqdefault / sddefault /
maxresdefault variant to default.jpg so the tabulated UI fetches
the smallest image."
  (or (and-let* ((url (alist-get 'url (car thumbnails)))
                 (qmark (string-search "?" url))
                 (base (substring url 0 qmark)))
        (replace-regexp-in-string
         "/\\(hq720\\|hqdefault\\|mqdefault\\|sddefault\\|maxresdefault\\)\\.jpg\\'"
         "/default.jpg" base))
      (format "https://i.ytimg.com/vi/%s/default.jpg" video-id)))

;;; lockupViewModel helpers (shared by video and playlist extraction)

(defun yeetube-scraper--lockup-metadata-parts (renderer)
  "Return metadataParts list from the first row of a lockupViewModel RENDERER."
  (let* ((meta (alist-get 'lockupMetadataViewModel
                          (alist-get 'metadata renderer)))
         (cmvm (alist-get 'contentMetadataViewModel
                          (alist-get 'metadata meta)))
         (rows (alist-get 'metadataRows cmvm)))
    (alist-get 'metadataParts (car rows))))

(defun yeetube-scraper--lockup-part-text (part)
  "Return the text content from a single metadataParts PART, or nil."
  (alist-get 'content (alist-get 'text part)))

(defun yeetube-scraper--lockup-views-and-date (parts)
  "Return cons (VIEWS . DATE) extracted from PARTS list.
The part whose text contains \"view\" is the view count; the
remaining part is the published date.  Returns empty strings for
absent fields."
  (let (views date)
    (dolist (part parts)
      (let ((text (yeetube-scraper--lockup-part-text part)))
        (cond ((null text))
              ((string-match-p "view" text) (setq views text))
              (t (setq date (or date text))))))
    (cons (or views "") (or date ""))))

(defun yeetube-scraper--lockup-duration (renderer)
  "Return duration string from RENDERER's bottom-overlay badge, or nil."
  (let ((overlays (alist-get 'overlays
                             (alist-get 'thumbnailViewModel
                                        (alist-get 'contentImage renderer)))))
    (cl-some (lambda (ov)
               (when-let* ((bot (alist-get 'thumbnailBottomOverlayViewModel ov))
                           (badge (car (alist-get 'badges bot)))
                           (text (alist-get 'text
                                            (alist-get 'thumbnailBadgeViewModel
                                                       badge))))
                 text))
             overlays)))

(defun yeetube-scraper--lockup-thumbnails (renderer)
  "Return thumbnail sources list from a lockupViewModel RENDERER."
  (alist-get 'sources
             (alist-get 'image
                        (alist-get 'thumbnailViewModel
                                   (alist-get 'contentImage renderer)))))

(defun yeetube-scraper--extract-video-lockup (renderer)
  "Extract a video plist from a VIDEO-type lockupViewModel RENDERER alist.
YouTube migrated channel-tab video rows from videoRenderer to this
lockup shape in 2025."
  (let* ((id (alist-get 'contentId renderer))
         (title (alist-get 'content
                           (alist-get 'title
                                      (alist-get 'lockupMetadataViewModel
                                                 (alist-get 'metadata renderer)))))
         (parts (yeetube-scraper--lockup-metadata-parts renderer))
         (vd (yeetube-scraper--lockup-views-and-date parts))
         (thumb-url (yeetube-scraper--thumbnail-url
                     id (yeetube-scraper--lockup-thumbnails renderer))))
    (list :id id
          :title (or title "")
          :views (car vd)
          :duration (or (yeetube-scraper--lockup-duration renderer) "")
          :date (cdr vd)
          :channel ""
          :channel-id ""
          :browse-id ""
          :thumbnail-url (or thumb-url "")
          :type 'video)))

;;; Playlist extraction (lockupViewModel)

(defun yeetube-scraper--row-text (row)
  "Extract the content string from a metadata ROW."
  (alist-get 'content
	     (alist-get 'text
			(car (alist-get 'metadataParts row)))))

(defun yeetube-scraper--playlist-channel (metadata-rows)
  "Extract channel name from playlist METADATA-ROWS."
  (yeetube-scraper--row-text (car metadata-rows)))

(defun yeetube-scraper--playlist-video-count (metadata-rows)
  "Extract video count string from playlist METADATA-ROWS."
  (when-let* ((found (cl-find-if
                      (lambda (row)
                        (and-let* ((text (yeetube-scraper--row-text row)))
                          (string-match-p "video" text)))
                      metadata-rows)))
    (yeetube-scraper--row-text found)))

(defun yeetube-scraper--playlist-thumbnail (renderer)
  "Extract thumbnail URL from playlist RENDERER."
  (let* ((image (alist-get 'contentImage renderer))
         (collection (alist-get 'collectionThumbnailViewModel image))
         (primary (alist-get 'primaryThumbnail collection))
         (thumb-vm (alist-get 'thumbnailViewModel primary))
         (sources (alist-get 'sources (alist-get 'image thumb-vm))))
    (alist-get 'url (car sources))))

(defun yeetube-scraper--extract-playlist (renderer)
  "Extract a playlist plist from a lockupViewModel RENDERER alist."
  (let* ((meta (alist-get 'lockupMetadataViewModel
			  (alist-get 'metadata renderer)))
         (title (alist-get 'content (alist-get 'title meta)))
         (rows (alist-get 'metadataRows
			  (alist-get 'contentMetadataViewModel
				     (alist-get 'metadata meta))))
         (channel (yeetube-scraper--playlist-channel rows))
         (video-count (yeetube-scraper--playlist-video-count rows))
         (thumb-url (yeetube-scraper--playlist-thumbnail renderer)))
    (list :id (alist-get 'contentId renderer)
          :title (or title "")
          :views ""
          :duration (or video-count "")
          :date ""
          :channel (or channel "")
          :channel-id ""
          :thumbnail-url (or thumb-url "")
          :type 'playlist)))

;;; Item dispatch

(defun yeetube-scraper--dispatch-item (item)
  "Dispatch ITEM to the appropriate extractor.  Return plist or nil."
  (cond
   ((alist-get 'videoRenderer item)
    (yeetube-scraper--extract-video (alist-get 'videoRenderer item)))
   ;; YouTube migrated playlists, and later channel-tab video rows, to
   ;; lockupViewModel.  Branch on contentType.
   ((alist-get 'lockupViewModel item)
    (let ((lockup (alist-get 'lockupViewModel item)))
      (pcase (alist-get 'contentType lockup)
        ("LOCKUP_CONTENT_TYPE_PLAYLIST"
         (yeetube-scraper--extract-playlist lockup))
        ("LOCKUP_CONTENT_TYPE_VIDEO"
         (yeetube-scraper--extract-video-lockup lockup)))))))

;;; Continuation token extraction

(defun yeetube-scraper--extract-continuation (sections)
  "Extract continuation token from SECTIONS.
Return plist (:token T :url U) or nil."
  (let ((cont-item
         (cl-find-if
          (lambda (s) (alist-get 'continuationItemRenderer s))
          sections)))
    (when cont-item
      (let* ((renderer (alist-get 'continuationItemRenderer cont-item))
             (endpoint (alist-get 'continuationEndpoint renderer))
             (token (alist-get 'token
			       (alist-get 'continuationCommand endpoint)))
             (url (alist-get 'apiUrl
			     (alist-get 'webCommandMetadata
					(alist-get 'commandMetadata endpoint)))))
        (when token
          (list :token token :url (or url "")))))))

;;; Section / grid item extraction

(defun yeetube-scraper--extract-section-items (sections)
  "Extract item plists from search result SECTIONS."
  (let* ((item-section
          (cl-find-if
           (lambda (s) (alist-get 'itemSectionRenderer s))
           sections))
         (items (alist-get 'contents
			   (alist-get 'itemSectionRenderer item-section))))
    (cl-loop for item in items
             for plist = (yeetube-scraper--dispatch-item item)
             when plist collect plist)))

(defun yeetube-scraper--extract-grid-items (grid-contents)
  "Extract item plists from channel page GRID-CONTENTS.
Each grid entry is wrapped in a `richItemRenderer'; the inner
`content' is then either a videoRenderer (legacy) or a
lockupViewModel (current YouTube layout), so dispatch through
`yeetube-scraper--dispatch-item' to handle both."
  (cl-loop for entry in grid-contents
           for inner = (alist-get 'content (alist-get 'richItemRenderer entry))
           for plist = (and inner (yeetube-scraper--dispatch-item inner))
           when plist collect plist))

;;; Page-type parsers

(defun yeetube-scraper--parse-search (contents)
  "Parse search results from CONTENTS alist.
Return plist (:items ITEMS :continuation CONT)."
  (let* ((primary (alist-get 'primaryContents
			     (alist-get 'twoColumnSearchResultsRenderer contents)))
         (sections (alist-get 'contents
			      (alist-get 'sectionListRenderer primary)))
         (items (yeetube-scraper--extract-section-items sections))
         (continuation (yeetube-scraper--extract-continuation sections)))
    (list :items items :continuation continuation)))

(defun yeetube-scraper--parse-channel (contents)
  "Parse channel page from CONTENTS alist.
Return plist (:items ITEMS :continuation CONT)."
  (let* ((tabs (alist-get 'tabs
			  (alist-get 'twoColumnBrowseResultsRenderer contents)))
         (selected (cl-find-if
                    (lambda (tab)
                      (eq t (alist-get 'selected
				       (alist-get 'tabRenderer tab))))
                    tabs))
         (grid-contents
          (thread-last
            (alist-get 'tabRenderer selected)
            (alist-get 'content)
            (alist-get 'richGridRenderer)
            (alist-get 'contents)))
         (items (yeetube-scraper--extract-grid-items grid-contents))
         (continuation (yeetube-scraper--extract-continuation grid-contents)))
    (list :items items :continuation continuation)))

;;; Top-level buffer parser

(defun yeetube-scraper-parse ()
  "Parse ytInitialData from the current buffer.
Return plist (:items ITEM-PLISTS :continuation (:token T :url U)).
Point is restored after parsing."
  (save-excursion
    (goto-char (point-min))
    (search-forward "ytInitialData")
    (search-forward "=")
    (skip-chars-forward " \t\n")
    (let* ((json (json-parse-buffer :object-type 'alist :array-type 'list))
           (contents (alist-get 'contents json)))
      (cond
       ((alist-get 'twoColumnSearchResultsRenderer contents)
        (yeetube-scraper--parse-search contents))
       ((alist-get 'twoColumnBrowseResultsRenderer contents)
        (yeetube-scraper--parse-channel contents))
       (t (list :items nil :continuation nil))))))

;;; Continuation response parser

(defun yeetube-scraper-parse-continuation-response (json)
  "Parse a continuation/pagination JSON response.
Return (:items ... :continuation ...)."
  (let* ((commands (alist-get 'onResponseReceivedCommands json))
         (action (alist-get 'appendContinuationItemsAction (car commands)))
         (cont-items (alist-get 'continuationItems action)))
    (list :items (yeetube-scraper--extract-section-items cont-items)
          :continuation (yeetube-scraper--extract-continuation cont-items))))

(provide 'yeetube-scraper)
;;; yeetube-scraper.el ends here
