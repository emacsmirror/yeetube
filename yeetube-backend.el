;;; yeetube-backend.el --- Generic backend interface for yeetube  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

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

;; Generic interface between the yeetube front-end and content
;; backends.  A backend is a symbol; each capability below is a
;; `cl-defgeneric' dispatching on it with an EQL specializer, so adding
;; a backend means implementing these methods in a new module (see
;; yeetube-youtube.el for the reference implementation).
;;
;; Backends describe HTTP requests as plists so the front-end owns all
;; transport (Tor routing, headers, async retrieval):
;;
;;   (:url URL [:method METHOD] [:headers ALIST] [:data STRING])
;;
;; Parsers are called with the decoded response body in a temporary
;; buffer and return item plists as documented in yeetube-scraper.el.

;;; Code:

(require 'cl-generic)

(defcustom yeetube-backend 'youtube
  "Active content backend."
  :type '(choice (const :tag "YouTube" youtube)
                 (symbol :tag "Other"))
  :group 'yeetube)

;;; Requests

(cl-defgeneric yeetube-backend-search-request (backend query)
  "Return a request spec searching BACKEND for QUERY.")

(cl-defgeneric yeetube-backend-channel-request (backend channel what &optional query)
  "Return a request spec for CHANNEL content on BACKEND.
WHAT is `videos', `streams', or `search' with a QUERY string.")

(cl-defgeneric yeetube-backend-continuation-request (backend continuation)
  "Return a request spec fetching the next page on BACKEND.
CONTINUATION is the opaque value a parser returned for the
previous page.")

;;; Parsers

(cl-defgeneric yeetube-backend-parse-page (backend)
  "Parse a BACKEND results page from the current buffer.
Return a plist (:items ITEMS :continuation CONTINUATION), where
CONTINUATION is nil or an opaque value for
`yeetube-backend-continuation-request'.")

(cl-defgeneric yeetube-backend-parse-continuation (backend)
  "Parse a BACKEND continuation response from the current buffer.
Return a plist (:items ITEMS :continuation CONTINUATION).")

(cl-defgeneric yeetube-backend-parse-feed (_backend)
  "Parse a BACKEND channel feed from the current buffer.
Return a list of item plists, nil when feeds are unsupported."
  nil)

;;; URLs

(cl-defgeneric yeetube-backend-item-url (backend id type)
  "Return BACKEND playback URL for item ID of TYPE (`video' or `playlist').")

(cl-defgeneric yeetube-backend-browse-url (backend id type)
  "Return BACKEND URL for viewing item ID of TYPE in a web browser."
  (yeetube-backend-item-url backend id type))

(cl-defgeneric yeetube-backend-feed-url (_backend _channel)
  "Return BACKEND feed URL for CHANNEL, nil when feeds are unsupported."
  nil)

;;; Prompts

(cl-defgeneric yeetube-backend-read-channel (_backend)
  "Interactively read a BACKEND channel identifier."
  (let ((channel (string-trim (read-string "Channel: "))))
    (if (string-empty-p channel)
        (user-error "No channel specified")
      channel)))

(provide 'yeetube-backend)
;;; yeetube-backend.el ends here
