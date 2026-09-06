;;; yeetube-ol.el --- Yeetube org-link integration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Steven Allen
;; Copyright (C) 2026  Thanos Apollo

;; Author: Steven Allen <steven@stebalien.com>
;; Maintainer: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos org
;; URL: https://git.thanosapollo.org/yeetube

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

;; Org-link types `yt-video:' and `yt-playlist:' for storing,
;; following, and exporting links to yeetube entries.
;;
;; Format: yt-video:VIDEO-ID and yt-playlist:PLAYLIST-ID
;; Example: [[yt-video:dQw4w9WgXcQ][Some video]]
;;
;; The link types are registered when this file is loaded.  To
;; activate, add to your init:
;;
;;   (with-eval-after-load 'org (require 'yeetube-ol))

;;; Code:

(require 'ol)
(require 'tabulated-list)
(require 'yeetube)

;;; Helpers

(defun yeetube-ol--store-link (type)
  "Store an Org link to the current yeetube item of TYPE.
TYPE is `video' or `playlist'.  Does nothing unless the current
buffer is in `yeetube-mode' and the item at point matches TYPE."
  (when (derived-mode-p 'yeetube-mode)
    (let* ((id (or (tabulated-list-get-id)
                   (save-excursion (end-of-line) (tabulated-list-get-id))))
           (item (yeetube--find-item id))
           (title (plist-get item :title)))
      (when (eq (plist-get item :type) type)
        (org-link-store-props :type (format "yt-%S" type)
                              :link (format "yt-%S:%s" type id)
                              :description title)))))

(declare-function org-html-encode-plain-text "ox-html" (text))
(declare-function org-md-plain-text "ox-md" (text info))
(declare-function org-latex-plain-text "ox-latex" (text info))
(declare-function org-latex--protect-text "ox-latex" (text))

(defun yeetube-ol--encode-url (url regexp)
  "Percent-encode characters matching REGEXP in URL.
Leave existing escapes and URL separators intact."
  (replace-regexp-in-string
   regexp
   (lambda (char)
     (mapconcat (lambda (byte) (format "%%%02X" byte))
                (encode-coding-string char 'utf-8 t) ""))
   url t t))

(defun yeetube-ol--html-text (text)
  "Escape TEXT for HTML text or a double-quoted attribute."
  ;; Org's plain-text encoder does not escape attribute delimiters.
  (replace-regexp-in-string
   "\"" "&quot;" (org-html-encode-plain-text text) t t))

(defun yeetube-ol--export (url desc backend)
  "Export URL with description DESC to BACKEND.
DESC is already exported by Org.  When nil, escape URL as plain text
for the fallback label, separately from the link destination."
  (pcase backend
    ('html
     (require 'ox-html)
     (format "<a href=\"%s\">%s</a>"
             (yeetube-ol--html-text url)
             (or desc (yeetube-ol--html-text url))))
    ('md
     (require 'ox-md)
     (format "[%s](%s)"
             (or desc
                 (replace-regexp-in-string
                  "[][]" "\\\\\\&"
                  (org-md-plain-text (org-html-encode-plain-text url) nil)))
             (org-html-encode-plain-text
              (yeetube-ol--encode-url url "[][()<>\"\\\\{}[:space:][:cntrl:]]"))))
    ('latex
     (require 'ox-latex)
     ;; Encoding braces and backslashes avoids TeX command/group syntax;
     ;; protect the remaining URL characters just as native Org links do.
     (format "\\href{%s}{%s}"
             (org-latex--protect-text
              (yeetube-ol--encode-url url "[\\\\{}[:space:][:cntrl:]]"))
             (or desc (org-latex-plain-text url nil))))
    (_ (or desc url))))

;;; Store / follow / export

(defun yeetube-ol-store-video-link (&optional _interactive)
  "Store an Org link to the yeetube video at point."
  (yeetube-ol--store-link 'video))

(defun yeetube-ol-follow-video (path _prefix)
  "Play the yeetube video with id PATH."
  (funcall yeetube-play-function
           (yeetube-backend-item-url yeetube-backend path 'video)))

(defun yeetube-ol-export-video (path desc backend _channel)
  "Export a yt-video: link to BACKEND.
PATH is the video id; DESC the user-visible label."
  (yeetube-ol--export (yeetube-backend-item-url yeetube-backend path 'video)
                      desc backend))

(defun yeetube-ol-store-playlist-link (&optional _interactive)
  "Store an Org link to the yeetube playlist at point."
  (yeetube-ol--store-link 'playlist))

(defun yeetube-ol-follow-playlist (path _prefix)
  "Display the yeetube playlist with id PATH."
  (yeetube--display-loading)
  (yeetube-display-content-from-url
   (yeetube-backend-item-url yeetube-backend path 'playlist)))

(defun yeetube-ol-export-playlist (path desc backend _channel)
  "Export a yt-playlist: link to BACKEND.
PATH is the playlist id; DESC the user-visible label."
  (yeetube-ol--export (yeetube-backend-item-url yeetube-backend path 'playlist)
                      desc backend))

(org-link-set-parameters "yt-video"
                         :store #'yeetube-ol-store-video-link
                         :follow #'yeetube-ol-follow-video
                         :export #'yeetube-ol-export-video)

(org-link-set-parameters "yt-playlist"
                         :store #'yeetube-ol-store-playlist-link
                         :follow #'yeetube-ol-follow-playlist
                         :export #'yeetube-ol-export-playlist)

(provide 'yeetube-ol)
;;; yeetube-ol.el ends here
