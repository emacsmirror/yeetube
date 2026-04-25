;;; yeetube-menu.el --- Yeetube transient menus  -*- lexical-binding: t; -*-

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

;; Transient menus for yeetube.

;;; Code:

(require 'transient)

;; Forward declarations -- yeetube.el
(declare-function yeetube-search "yeetube")
(declare-function yeetube-replay "yeetube")
(declare-function yeetube-play "yeetube")
(declare-function yeetube-play-saved-video "yeetube")
(declare-function yeetube-download-video "yeetube")
(declare-function yeetube-save-video "yeetube")
(declare-function yeetube-copy-url "yeetube")
(declare-function yeetube-browse-url "yeetube")
(declare-function yeetube-channel-videos "yeetube")
(declare-function yeetube-channel-streams "yeetube")
(declare-function yeetube-next-page "yeetube")
(declare-function yeetube-set-results-limit "yeetube")
(defvar yeetube-results-limit)
(defvar yeetube--results-limit)
(defvar yeetube-download-directory)
(defvar yeetube-download-audio-format)

;; Forward declarations -- yeetube-download.el
(declare-function yeetube-download-change-directory "yeetube-download")
(declare-function yeetube-download-change-audio-format "yeetube-download")
(declare-function yeetube-download-videos "yeetube-download")

;; Forward declarations -- yeetube-mpv.el
(declare-function yeetube-mpv-change-video-quality "yeetube-mpv")
(declare-function yeetube-mpv-toggle-torsocks "yeetube-mpv")
(declare-function yeetube-mpv-toggle-pause "yeetube-mpv")
(declare-function yeetube-mpv-toggle-video "yeetube-mpv")
(declare-function yeetube-mpv-toggle-no-video-flag "yeetube-mpv")
(defvar yeetube-mpv-video-quality)
(defvar yeetube-mpv-enable-torsocks)


;;; ---- Setting suffixes ----

(defun yeetube-menu--value-str (value)
  "Format VALUE as a propertized string for transient display."
  (propertize (format "%s" (or value "none"))
              'face 'transient-value))

(transient-define-suffix yeetube-menu--set-limit ()
  "Set results limit and re-fetch."
  :transient t
  :description (lambda ()
                 (format "Results limit %s"
                         (yeetube-menu--value-str
                          (with-current-buffer (get-buffer-create "*yeetube*")
                            (or yeetube--results-limit yeetube-results-limit)))))
  (interactive)
  (let ((limit (read-number "Results limit: "
                            (with-current-buffer (get-buffer-create "*yeetube*")
                              (or yeetube--results-limit yeetube-results-limit)))))
    (with-current-buffer "*yeetube*"
      (yeetube-set-results-limit limit))))

(transient-define-suffix yeetube-menu--set-quality ()
  "Set video quality."
  :transient t
  :description (lambda ()
                 (format "Video quality %s" (yeetube-menu--value-str yeetube-mpv-video-quality)))
  (interactive)
  (yeetube-mpv-change-video-quality))

(transient-define-suffix yeetube-menu--toggle-torsocks ()
  "Toggle torsocks."
  :transient t
  :description (lambda ()
                 (format "Torsocks %s"
                         (yeetube-menu--value-str (if yeetube-mpv-enable-torsocks "on" "off"))))
  (interactive)
  (yeetube-mpv-toggle-torsocks))

(transient-define-suffix yeetube-menu--set-download-dir ()
  "Change download directory."
  :transient t
  :description (lambda ()
                 (format "Download dir %s"
                         (yeetube-menu--value-str
                          (abbreviate-file-name yeetube-download-directory))))
  (interactive)
  (yeetube-download-change-directory))

(transient-define-suffix yeetube-menu--set-audio-format ()
  "Change audio format."
  :transient t
  :description (lambda ()
                 (format "Audio format %s"
                         (yeetube-menu--value-str yeetube-download-audio-format)))
  (interactive)
  (yeetube-download-change-audio-format
   (completing-read "Audio format (no for nil): "
                    '("no" "aac" "alac" "flac" "m4a" "mp3" "opus" "vorbis" "wav")
                    nil t)))


;;; ---- Transient prefixes ----

;;;###autoload
(transient-define-prefix yeetube-menu ()
  "YeeTube."
  [["Search"
    ("s" "Search" yeetube-search)
    ("r" "Replay" yeetube-replay)
    ("P" "Play saved" yeetube-play-saved-video)]
   ["Download"
    ("d" "Download video" yeetube-download-video)
    ("U" "Bulk download" yeetube-download-videos)]
   ["Settings"
    ("n" yeetube-menu--set-limit)
    ("Q" yeetube-menu--set-quality)
    ("T" yeetube-menu--toggle-torsocks)
    ("D" yeetube-menu--set-download-dir)
    ("a" yeetube-menu--set-audio-format)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix yeetube-buffer-menu ()
  "YeeTube buffer actions."
  [["Play"
    ("RET" "Play" yeetube-play)
    ("r" "Replay" yeetube-replay)
    ("P" "Play saved" yeetube-play-saved-video)]
   ["Navigate"
    ("s" "Search" yeetube-search)
    ("M-n" "Next page" yeetube-next-page)
    ("c" "Channel videos" yeetube-channel-videos)
    ("L" "Channel streams" yeetube-channel-streams)
]
   ["Actions"
    ("S" "Save video" yeetube-save-video)
    ("C" "Copy URL" yeetube-copy-url)
    ("b" "Browse (invidious)" yeetube-browse-url)
    ("d" "Download" yeetube-download-video)]
   ["MPV"
    ("p" "Toggle pause" yeetube-mpv-toggle-pause)
    ("v" "Toggle video" yeetube-mpv-toggle-video)
    ("V" "No-video flag" yeetube-mpv-toggle-no-video-flag)]
   ["Settings"
    ("n" yeetube-menu--set-limit)
    ("Q" yeetube-menu--set-quality)
    ("T" yeetube-menu--toggle-torsocks)
    ("D" yeetube-menu--set-download-dir)
    ("a" yeetube-menu--set-audio-format)
    ("q" "Quit" quit-window)]])

(provide 'yeetube-menu)
;;; yeetube-menu.el ends here
