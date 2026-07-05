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

(defun yeetube-ol--export (url desc backend)
  "Export URL with description DESC to BACKEND.
DESC falls back to URL when nil."
  (let ((desc (or desc url)))
    (pcase backend
      ('html (format "<a href=\"%s\">%s</a>" url desc))
      ('md (format "[%s](%s)" desc url))
      ('latex (format "\\href{%s}{%s}" url desc))
      (_ desc))))

;;; Store / follow / export

(defun yeetube-ol-store-video-link (&optional _interactive)
  "Store an Org link to the yeetube video at point."
  (yeetube-ol--store-link 'video))

(defun yeetube-ol-follow-video (path _prefix)
  "Play the yeetube video with id PATH."
  (funcall yeetube-play-function (concat yeetube-video-url path)))

(defun yeetube-ol-export-video (path desc backend _channel)
  "Export a yt-video: link to BACKEND.
PATH is the video id; DESC the user-visible label."
  (yeetube-ol--export (concat yeetube-video-url path) desc backend))

(defun yeetube-ol-store-playlist-link (&optional _interactive)
  "Store an Org link to the yeetube playlist at point."
  (yeetube-ol--store-link 'playlist))

(defun yeetube-ol-follow-playlist (path _prefix)
  "Display the yeetube playlist with id PATH."
  (yeetube--display-loading)
  (yeetube-display-content-from-url (concat yeetube-playlist-url path)))

(defun yeetube-ol-export-playlist (path desc backend _channel)
  "Export a yt-playlist: link to BACKEND.
PATH is the playlist id; DESC the user-visible label."
  (yeetube-ol--export (concat yeetube-playlist-url path) desc backend))

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
